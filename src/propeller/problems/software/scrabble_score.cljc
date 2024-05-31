;; scrabble_score.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a string of visible characters with length <= 20, return the Scrabble
;; score for that string. Each letter has a corresponding value according to
;; normal Scrabble rules, and non-letter character are worth zero.
;;
;; input stack has the input string

(ns propeller.problems.software.scrabble-score
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.metrics :as metrics]
            [clojure.string :as string]
            [propeller.tools.maxbandit :as maxbandit]
            [clojure.math.combinatorics :as combo]
            #?(:cljs [cljs.reader :refer [read-string]])))

(def scrabble-letter-values
  (let [scrabble-map {\a 1
                      \b 3
                      \c 3
                      \d 2
                      \e 1
                      \f 4
                      \g 2
                      \h 4
                      \i 1
                      \j 8
                      \k 5
                      \l 1
                      \m 3
                      \n 1
                      \o 1
                      \p 3
                      \q 10
                      \r 1
                      \s 1
                      \t 1
                      \u 1
                      \v 4
                      \w 4
                      \x 8
                      \y 4
                      \z 10}
        visible-chars (map char (range 0 127))]
    (vec (for [c visible-chars]
           (get scrabble-map (first (string/lower-case c)) 0)))))

; Atom generators
(def instructions
  (concat (list
            scrabble-letter-values
            ;;; end constants
            ;;; end tag ERCs
            :in1
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:string :char :integer :boolean :vector_integer :exec})))

;; Define test cases
(defn scrabble-score-input
  "Makes a Scrabble Score input of length len."
  [len]
  (apply str
         (repeatedly len
                     (fn []
                       (rand-nth (concat (list \newline \tab)
                                          (map char (range 32 127))))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def scrabble-score-data-domains
  [[(map (comp str char) (range 97 123)) 26 0] ;; Lowercase single letters
   [(map (comp str char) (range 65 91)) 0 26] ;; Uppercase single letters
   [(list "", "*", " ", "Q ", "zx", " Dw", "ef", "!!", " F@", "ydp", "4ps"
          "abcdefghijklmnopqrst"
          "ghijklmnopqrstuvwxyz"
          "zxyzxyqQQZXYqqjjawp"
          "h w h j##r##r\n+JJL"
          (apply str (take 13 (cycle (list \i \space \!))))
          (apply str (repeat 20 \Q))
          (apply str (repeat 20 \$))
          (apply str (repeat 20 \w))
          (apply str (take 20 (cycle (list \1 \space))))
          (apply str (take 20 (cycle (list \space \v))))
          (apply str (take 20 (cycle (list \H \a \space))))
          (apply str (take 20 (cycle (list \x \space \y \!))))
          (apply str (take 20 (cycle (list \G \5))))) 24 0] ;; "Special" inputs covering some base cases
   [(fn [] (scrabble-score-input (+ 2 (rand-int 19)))) 150 974] ;; Random strings with at least 2 characters
   ])

;;Can make Scrabble Score test data like this:
;(test-and-train-data-from-domains scrabble-score-data-domains)

; Helper function for error function
(defn scrabble-score-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (apply + (map #(nth scrabble-letter-values (int %)) in))))
       inputs))

(def error-function 
  (gp/default-error-function :input-fn (fn [input] {:in1 input})
                             :out-stacks :integer
                             :error (fn [target output]
                                      (if (number? output)
                                        (math/abs (- output target)) ;distance from correct integer
                                        1000))))

(defn get-scrabble-score-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map scrabble-score-test-cases
            (utils/test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def data
  (get-scrabble-score-train-and-test scrabble-score-data-domains))

(defn scrabble-score-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn scrabble-score-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Scrabble Score problem report - generation %s\n" generation)(flush)
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
     :max-initial-plushy-size 500
     :step-limit              2000
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
  (maxbandit/downsample-gp
   (merge
    {;;:mapper utils/pmapallv2
     :downsample?                 true ; wether to use downsampling
     :ds-function                 :case-maxmin ; :case-rand, case-maxmin, case-maxmin-auto
     :downsample-rate             0.05 ; proportion of data used in downsample
     :ds-parent-rate              0.01 ; proportion of parents used to evaluate case distances
     :ds-parent-gens              10 ; generations between computation of parent distances
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
     :transformations [maxbandit/tile-to-parameters-umad maxbandit/parameters-to-tile-umad]
     :bandit-parameters {:num-bandits 1
                         :l [-10]
                         :r [0]
                         :to [[0 0.03 0.06 0.09 0.12 0.15]] #_[[0 0.03 0.06 0.09 0.12 0.15 0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39 0.42 0.45 0.48]]
                         :ta [[0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39]] #_[[0.51 0.54 0.57 0.6 0.63 0.66 0.69 0.72 0.75 0.78 0.81 0.84 0.87 0.9]]
                         :num-codings 20
                         :d [30]
                         :acc [0.03]
                         :lr [{:method :momentum
                               :momentum-lr 0.03
                               :lr 0.0003
                               :dampening 1
                              ;;  :alpha 0.0001
                              ;;  :beta1 0.1
                              ;;  :beta2 0.001
                              ;;  :warm-start 5
                               }]
                         :s [50]
                         :delta [0.01]
                         :n 100
                         :mode [:softmax-scaled]
                         :AO true
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                        ;;  :dampening 1
                        ;;  :momentum-lr [0.03]
                         :max-n 300
                        ;;  :legacy true
                         }
     :generations-per-update 1
     :reward-transformation :log-diff
     :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.01} {:e 0.03} {:e 0.1} {:e 0.3} {:e 1} {:e 3}]
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
