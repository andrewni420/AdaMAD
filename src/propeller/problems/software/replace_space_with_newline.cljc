;; replace_space_with_newline.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a string input, print the string, replacing spaces with newlines.
;; The input string will not have tabs or newlines, but may have multiple spaces
;; in a row. It will have maximum length of 20 characters. Also, the program
;; should return the integer count of the non-whitespace characters.
;;
;; input stack has the input string

(ns propeller.problems.software.replace-space-with-newline
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.metrics :as metrics]
            [clojure.string :as string]
            [propeller.tools.gsemr :as gsemr]
            [propeller.tools.maxbandit2 :as maxbandit2]
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])))


;; Define test cases
(defn replace-space-with-newline-input
  "Makes a Replace Space With Newline input of length len."
  [len]
  (apply str
         (repeatedly len
                     (fn []
                       (if (< (rand) 0.2)
                         \space
                         (rand-nth (map char (range 32 127))))))))

; Atom generators
(def instructions
  (concat (list
           \space
           \newline
            ;;; end constants
           (fn [] (rand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
           (fn [] (replace-space-with-newline-input (rand-int 21))) ;String ERC
            ;;; end ERCs
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
(def replace-space-with-newline-data-domains
  [[(list "", "A", "*", " ", "s", "B ", "  ", " D", "ef", "!!", " F ", "T L", "4ps", "q  ", "   ", "  e", "hi ",
          "  $  ", "      9",
          (apply str (take 13 (cycle (list \i \space \!))))
          (apply str (repeat 20 \8))
          (apply str (repeat 20 \space))
          (apply str (repeat 20 \s))
          (apply str (take 20 (cycle (list \1 \space))))
          (apply str (take 20 (cycle (list \space \v))))
          (apply str (take 20 (cycle (list \H \a \space))))
          (apply str (take 20 (cycle (list \x \space \y \!))))
          (apply str (take 20 (cycle (list \G \5))))
          (apply str (take 20 (cycle (list \> \_ \= \]))))
          (apply str (take 20 (cycle (list \^ \_ \^ \space))))) 30 0] ;; "Special" inputs covering some base cases
   [(fn [] (replace-space-with-newline-input (+ 2 (rand-int 19)))) 70 1000]])

;;Can make Replace Space With Newline test data like this:
;(test-and-train-data-from-domains replace-space-with-newline-data-domains)




; Helper function for error function
(defn replace-space-with-newline-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 [(string/replace in \space \newline)
                  (count (filter #(not= \space %) in))]))
       inputs))

(defn get-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map replace-space-with-newline-test-cases
            (utils/test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def data
  (get-train-and-test replace-space-with-newline-data-domains))

(def error-function
  (gp/default-error-function :input-fn (fn [input] {:in1 input})
                             :initialized-stacks {:print '("")}
                             :out-stacks [:print :integer]
                             :error (fn [[prn-target int-target] [prn-out int-out]]
                                      [(metrics/levenshtein-distance prn-target prn-out)
                                       (if (number? int-out)
                                         (math/abs (- int-out int-target)) ;distance from correct integer
                                         1000)])))

(defn replace-space-with-newline-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))



(defn replace-space-with-newline-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-with-test (error-function best :test)
        best-test-errors (:test-errors best-with-test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Replace Space With Newline problem report - generation %s\n" generation) (flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (doseq [[[correct-output correct-int] [printed-result int-result]]
            (map vector
                 (map second (first data))
                 (partition 2 (:behaviors best)))]
      (println (format "\n| Correct output: %s\n| Program output: %s" (pr-str correct-output) (pr-str printed-result)))
      (println (format "| Correct integer: %2d | Program integer: %s" correct-int (str int-result))))
    (println ";;******************************")
    ;; return best individual with tests errors added so that those are recorded
    best-with-test))
       ;; To do validation, could have this function return an altered best individual
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

;; (defn test 
;;   []
;;   (println  (into [] (map (fn [lr] (math/pow 10 (- (/ lr 4.5) 4)))) (range 10))))

(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (maxbandit/gp
   (merge
    {;;:mapper mapv
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
                         :r [10]
                         :to [[0 0.03 0.06 0.09 0.12 0.15]] #_[[0 0.03 0.06 0.09 0.12 0.15 0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39 0.42 0.45 0.48]]
                         :ta [[0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39]] #_[[0.51 0.54 0.57 0.6 0.63 0.66 0.69 0.72 0.75 0.78 0.81 0.84 0.87 0.9]]
                         :num-codings 20
                         :d [1]
                         :acc [0.03]
                         :lr (into [] (map (fn [lr] {:method :momentum :lr (math/pow 10 (- (/ lr 9) 4)) :dampening 1 :momentum-lr 0.1})) (range 10))  
                         #_[{:method :momentum
                               :momentum-lr 0.1
                               :lr 0.0001
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
                         :AO true
                         :xi* [0.01]
                         :epsilon [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0.01)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                        ;;  :dampening 1
                        ;;  :momentum-lr [0.03]
                         :max-n 100
                        ;;  :legacy true
                         :sigma 3
                         }
     :generations-per-update 1
     :reward-transformation :log-diff
    ;;  :parameter-override {:e 0.0001}
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
  (gsemr/gp
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
     :rate-sigma 1.3
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))

