;; vectors_summed.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given two vectors of integers in [-1000,1000] of the same length <= 50,
;; return a vector of integers that sums the other two at each index.
;;
;; input stack has 2 input vectors of integers

(ns propeller.problems.software.vectors-summed
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.metrics :as metrics]
            [clojure.string :as string]
            #?(:cljs [cljs.reader :refer [read-string]])
            [propeller.push.instructions :as instructions]))

; Atom generators
(def instructions
  (concat (list
            []
            ;;; end constants
            (fn [] (- (rand-int 2001) 1000)) ;Integer ERC [-1000,1000]
            ;;; end tag ERCs
            :in1
            :in2
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :vector_integer :exec})))


;; Define test cases
(defn vectors-summed-input
  "Makes a pair of Vectors Summed input vectors of length len."
  [len]
  (vector (vec (repeatedly len
                           #(- (rand-int 2001) 1000)))
          (vec (repeatedly len
                           #(- (rand-int 2001) 1000)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def vectors-summed-data-domains
  [[(list [[] []]) 1 0] ;; Empty vectors
   [(concat (list [[0] [0]]
                  [[0] [10]]
                  [[3] [5]]
                  [[7] [-9]]
                  [[-432] [-987]])
            (repeatedly 5 #(vectors-summed-input 1))) 10 0] ;; Length 1 vectors
   [(list [[0 0] [0 0]]
          [[0 1] [-4 2]]
          [[-1 0] [-3 0]]
          [[-90 -6] [-323 49]]) 4 0] ;; Length 2 vectors
   [(fn [] (vectors-summed-input 50)) 10 100] ;; Length 50 vectors
   [(fn [] (vectors-summed-input (inc (rand-int 50)))) 125 1400] ;; Random length vectors
   ])

;;Can make Vectors Summed test data like this:
;(test-and-train-data-from-domains vectors-summed-data-domains)

; Helper function for error function
(defn vectors-summed-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (vec (map + (first in) (second in)))))
       inputs))

(def error-function 
  (gp/default-error-function :out-stacks :vector_integer
                             :error (fn [target output]
                                      (if (vector? output)
                                        (+' (apply +' (map (fn [cor res]
                                                             (math/abs (- cor res)))
                                                           target
                                                           output))
                                            (*' 10000 (math/abs (- (count target) (count output))))) ; penalty of 10000 times difference in sizes of vectors
                                        1000000000))))

(defn get-vectors-summed-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map vectors-summed-test-cases
       (utils/test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def data
  (get-vectors-summed-train-and-test vectors-summed-data-domains))

(defn vectors-summed-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn vectors-summed-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Vectors Summed problem report - generation %s\n" generation)(flush)
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


(defn -main
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
     :max-initial-plushy-size 250
     :step-limit              1500
     :restart 0.0
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.01
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
