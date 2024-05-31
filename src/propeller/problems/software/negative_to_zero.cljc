;; negative_to_zero.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of integers in [-1000,1000] with length <= 50, return the
;; vector where all negative integers have been replaced by 0.
;;
;; NOTE: This problem gets lots of solutions that don't generalize. We could add
;; another error that finds the integer error at each position in vector with
;; penalty for wrong size of vector, which might help with generalization (but might not).
;;
;; input stack has 1 input vector of integers

(ns propeller.problems.software.negative-to-zero
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.metrics :as metrics]
            [clojure.string :as string]
            [propeller.variation :as variation]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as numeric]
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])))

; Atom generators
(def instructions
  (concat (list
            0
            []
            ;;; end constants
            ;;; end ERCs
            :in1
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :vector_integer :exec})))


;; Define test cases
(defn negative-to-zero-input
  "Makes a Negative To Zero input vector of length len with probability prob of being negative."
  [len prob]
  (vec (repeatedly len
                   #(if (< (rand) prob)
                      (- (inc (rand-int 1000)))
                      (rand-int 1001)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def negative-to-zero-data-domains
  [[(list []) 1 0] ;; Empty vector
   [(concat (list [-10] [-1] [0] [1] [10])
            (repeatedly 5 #(vector (- (rand-int 2001) 1000)))) 10 0] ;; Length 1 vectors
   [(list [0 0]
          [0 1]
          [-1 0]
          [-90 -6]
          [-16 33]
          [412 111]) 6 0] ;; Length 2 vectors
   [(fn [] (negative-to-zero-input (inc (rand-int 50)) 1.0)) 9 100] ;; Random length, all negative
   [(fn [] (negative-to-zero-input (inc (rand-int 50)) 0.0)) 9 100] ;; Random length, all positive
   [(fn [] (negative-to-zero-input (inc (rand-int 50)) (rand))) 165 1800] ;; Random length, random prob of negative
   ])

;;Can make Negative To Zero test data like this:
;(test-and-train-data-from-domains negative-to-zero-data-domains)

; Helper function for error function
(defn negative-to-zero-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (vec (map #(if (< % 0) 0 %)
                           in))))
       inputs))

(def error-function 
  (gp/default-error-function :input-fn (fn [input] {:in1 input})
                             :out-stacks :vector_integer
                             :error (fn [target output]
                                      (if (vector? output)
                                        (metrics/levenshtein-distance target output)
                                        5000))))

(defn get-negative-to-zero-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map negative-to-zero-test-cases
       (utils/test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def data
  (get-negative-to-zero-train-and-test negative-to-zero-data-domains))

(defn negative-to-zero-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn negative-to-zero-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Negative To Zero problem report - generation %s\n" generation)(flush)
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
     :population-size         100
     :max-initial-plushy-size 250
     :step-limit              1500
     :restart 0.0
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
                         :d [1]
                         :acc [0.03]
                         :lr [{:method :momentum
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
                         :mode [:softmax-scaled]
                         :AO true
                         :xi* [0.01]
                         :epsilon [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                        ;;  :dampening 1
                        ;;  :momentum-lr [0.03]
                         :max-n 100
                        ;;  :legacy true
                         :sigma 3}
     :generations-per-update 1
     :reward-transformation :log-diff
     :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.01} {:e 0.03} {:e 0.1} {:e 0.3} {:e 1} {:e 3}]
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))

