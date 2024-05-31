;; syllables.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given a string (max length 20, containing symbols, spaces, digits, and
;; lowercase letters), count the number of occurrences of vowels (a,e,i,o,u,y)
;; in the string and print that number as X in "The number of syllables is X"
;;
;; input stack has the input string

(ns propeller.problems.software.syllables
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.metrics :as metrics]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as numeric]
            [propeller.tools.maxbandit :as maxbandit]
            [propeller.tools.gesmr :as gesmr]
            #?(:cljs [cljs.reader :refer [read-string]]))
  (:import java.lang.Integer
           java.lang.Exception))

;; Define test cases
(defn syllables-input
  "Makes a Syllables input of length len."
  [len]
  (apply str
         (repeatedly len
                     #(if (< (rand) 0.2)
                        (rand-nth "aeiouy")
                        (rand-nth (map char (concat (range 32 65) (range 91 127))))))))

; Atom generators
(def instructions
  (concat (list
            "The number of syllables is "
            \a
            \e
            \i
            \o
            \u
            \y
            "aeiouy"
            ;;; end constants
            (fn [] (rand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            (fn [] (syllables-input (rand-int 21))) ;String ERC
            ;;; end tag ERCs
            :in1
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :string :char :exec :print})))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def syllables-data-domains
  [[(list "", "a", "v", "4", "o", " ", "aei", "ouy", "chf", "quite", "a r e9j>"
          "you are many yay yea"
          (apply str (repeat 20 \s))
          (apply str (repeat 20 \o))
          (apply str (take 20 (cycle (list \w \i \space))))
          (apply str (take 20 (cycle (list \x \space \y \space))))
          (apply str (take 20 (cycle (list \e \i \o \y))))) 17 0] ;; Hand-chosen edge cases
   [(fn [] (syllables-input (inc (rand-int 20)))) 83 1000]
   ])

;;Can make syllables test data like this:
;(test-and-train-data-from-domains syllables-data-domains)

; Helper function for error function
(defn syllables-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (str "The number of syllables is " (count (filter #(some #{%} "aeiouy") in)))))
       inputs))

(def error-function
  (gp/default-error-function :input-fn (fn [input] {:in1 input})
                             :initialized-stacks {:print '("")}
                             :error (fn [target output]
                                      [(metrics/levenshtein-distance target output)
                                       (if-let [num-result (try (Integer/parseInt (last (string/split output #"\s+")))
                                                                (catch Exception e nil))]
                                         (math/abs (- (Integer/parseInt (last (string/split target #"\s+")))
                                                      num-result)) ;distance from correct integer
                                         1000)])))

(defn get-syllables-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map syllables-test-cases
            (utils/test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def data
  (get-syllables-train-and-test syllables-data-domains))

(defn syllables-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn syllables-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Syllables problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.



#_(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (gp/gp
   (merge
    {:instructions            instructions
     :error-function          error-function
     :training-data           (first data)
     :testing-data            (second data)
     :max-generations         300
     :population-size         1000
     :max-initial-plushy-size 400
     :step-limit              1600
     :restart 0.0
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.01
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))



(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (gesmr/gp
   (merge
    {#_:mapper #_mapv
    ;;  :downsample?                 true ; wether to use downsampling
    ;;  :ds-function                 :case-maxmin ; :case-rand, case-maxmin, case-maxmin-auto
    ;;  :downsample-rate             0.05 ; proportion of data used in downsample
    ;;  :ds-parent-rate              0.01 ; proportion of parents used to evaluate case distances
    ;;  :ds-parent-gens              10 ; generations between computation of parent distances
     :instructions            instructions
     :error-function          error-function
     :training-data           (first data)
     :testing-data            (second data)
     :max-generations         300
     :population-size         [1000 1000]
     :max-initial-plushy-size 400
     :step-limit              1600
     :parent-selection        :lexicase
     :tournament-size         5
     :update-freq 100
     :report-interval 1
     :transformations [maxbandit/tile-to-parameters-umad maxbandit/parameters-to-tile-umad]
     :bandit-parameters {:num-bandits 5
                         :l [-10]
                         :r [0]
                         :to [[0 0.03 0.06 0.09 0.12 0.15]] #_[[0 0.03 0.06 0.09 0.12 0.15 0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39 0.42 0.45 0.48]]
                         :ta [[0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39]] #_[[0.51 0.54 0.57 0.6 0.63 0.66 0.69 0.72 0.75 0.78 0.81 0.84 0.87 0.9]]
                         :num-codings 20
                         :d [1]
                         :acc [0.03]
                         :lr (into [] (map (fn [lr] {:method :momentum :lr (math/pow 10 (- (/ lr 9) 4)) :dampening 1 :momentum-lr 0.1})) (range 10))   
                             #_[{:method :momentum
                               :momentum-lr 0.1
                               :lr 0.001
                               :dampening 1
                                                                              ;;  :alpha 0.0003
                                                                              ;;  :beta1 0.03
                                                                              ;;  :beta2 0.001
                                                                              ;;  :warm-start 5
                               }]
                         :s [50]
                         :delta [0.01]
                         :n 300
                         :mode [:argmax]
                         :AO false
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5.) (range 5)) (repeat 5995 0.01)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 5995 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                                                                        ;;  :dampening 1
                                                                        ;;  :momentum-lr [0.03]
                         :max-n 100
                         :sigma 3
                                                                        ;;  :legacy true
                         }
     :generations-per-update 1
     :reward-transformation :log-diff
    ;;  :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.01} {:e 0.03} {:e 0.1} {:e 0.3} {:e 1} {:e 3}]
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))



#_(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (gesmr/gp
   (merge
    {#_:mapper #_mapv
     :instructions            instructions
     :error-function          error-function
     :training-data           (first data)
     :testing-data            (second data)
     :max-generations         300
     :population-size         [1000 1000]
     :max-initial-plushy-size 100
     :step-limit              200
     :parent-selection        :lexicase
     :tournament-size         5
     :update-freq 100
     :report-interval 1
     :generations-per-update 1
     :reward-transformation :log-diff 
     :n-rates 100 
     :rate-eta 0.1
     :rate-sigma 2
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))

