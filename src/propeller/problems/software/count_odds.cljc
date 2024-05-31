;; count_odds.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of integers with length <= 50, with each
;; integer in [-1000,1000], return the number of integers that are odd.
;;
;; input stack has 1 input vector of integers

(ns propeller.problems.software.count-odds
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.metrics :as metrics]
            [propeller.tools.maxbandit :as maxbandit]
            [clojure.string :as string]
            #?(:cljs [cljs.reader :refer [read-string]])))

; Atom generators
(def instructions
  (concat (list
            0
            1
            2
            ;;; end constants
            (fn [] (- (rand-int 201) 100)) ;Integer ERC
            ;;; end tag ERCs
            :in1
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :vector_integer :exec})))


;; Define test cases
(defn count-odds-input
  "Makes a Count Odds input vector of length len with probability prob of being odd."
  [len prob]
  (vec (repeatedly len
                   #(if (< (rand) prob)
                      (inc (* 2 (- (rand-int 1000) 500)))
                      (* 2 (- (rand-int 1001) 500))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def count-odds-data-domains
  [[(list []) 1 0] ;; Empty vector
   [(concat (map vector (range -10 11))
            (list [-947] [-450] [303] [886])) 25 0] ;; Length 1 vectors
   [(list [0 0]
          [0 1]
          [7 1]
          [-9 -1]
          [-11 40]
          [944 77]) 6 0] ;; Length 2 vectors
   [(fn [] (count-odds-input (inc (rand-int 50)) 1.0)) 9 100] ;; Random length, all odd
   [(fn [] (count-odds-input (inc (rand-int 50)) 0.0)) 9 100] ;; Random length, all even
   [(fn [] (count-odds-input (inc (rand-int 50)) (rand))) 150 1800] ;; Random length, random prob of odd
   ])

;;Can make Count Odds test data like this:
;(test-and-train-data-from-domains count-odds-data-domains)

; Helper function for error function
(defn count-odds-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (count (filter odd? %)))
       inputs))

(def error-function 
  (gp/default-error-function :input-fn (fn [input] {:in1 input})
                             :out-stacks :integer
                             :error (fn [target output] 
                                      (if (number? output)
                                        (math/abs (- target output)) ; distance from correct integer
                                        1000))))



(defn get-count-odds-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map count-odds-test-cases
       (utils/test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def data
  (get-count-odds-train-and-test count-odds-data-domains))



(defn count-odds-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn count-odds-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Count Odds problem report - generation %s\n" generation)(flush)
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
  (gp/max-gp
   (merge
    {:instructions            instructions
     :error-function          error-function
     :training-data           (first data)
     :testing-data            (second data)
     :max-generations         300
     :population-size         [10000 5]
     :num-runs 10
     :max-initial-plushy-size 250
     :step-limit              1500
     :restart 0.0
     :epsilon 0.2
     :sampling-type :epsilon-greedy
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))




(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (maxbandit/gp
   (merge
    {#_:mapper #_mapv
     :instructions            instructions
     :error-function          error-function
     :training-data           (first data)
     :testing-data            (second data)
     :max-generations         300
     :population-size         [1000 1000]
     :max-initial-plushy-size 250
     :step-limit              2000
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
                         :d [1]
                         :acc [0.03]
                         :lr [{:method :adam
                              ;;  :momentum-lr 0.03
                              ;;  :lr 0.0003
                              ;;  :dampening 1
                               :alpha 0.0003
                               :beta1 0.03
                               :beta2 0.001
                              ;;  :warm-start 5
                               }]
                         :s [50]
                         :delta [0.01]
                         :n 300
                         :mode [:softmax-scaled]
                         :AO false
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                        ;;  :dampening 1
                        ;;  :momentum-lr [0.03]
                         :max-n 300
                         :sigma 2
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
