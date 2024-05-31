;; last_index_of_zero.clj
;; Nic McPhee, mcphee@morris.umn.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of integers of length <= 50, each integer in the range [-50,50],
;; at least one of which is 0, return the index of the last occurance of 0 in the vector.
;;
;; input stack has 1 input vector of integers

(ns propeller.problems.software.last-index-of-zero
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
            #?(:cljs [cljs.reader :refer [read-string]])
            [propeller.push.instructions :as instructions]))

; Atom generators
(def instructions
  (concat (list
            ^{:generator-label "Random numbers in the range [-50,50]"}
            (fn [] (- (rand-int 101) 50))
            ;;; end ERCs
            :in1
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :vector_integer :exec})))

;; Define test cases
(defn random-sequence-with-at-least-one-zero
  [max-extra-zeros max-additional-values]
  (shuffle
   (concat
    [0] ; To ensure at least one zero
    (repeat (rand-int (inc max-extra-zeros)) 0)
    (repeatedly (rand-int (inc max-additional-values)) #(- (rand-int 101) 50)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def last-index-of-zero-data-domains
  [^{:domain-label "length 2 vectors"}
   [(list [0 1]
          [1 0]
          [7 0]
          [0 8]
          [0 -1]
          [-1 0]
          [-7 0]
          [0 -8]) 8 0]
   ^{:domain-label "vectors of all zeros"}
   [(map #(vec (repeat (inc %) 0)) (range 50)) 30 20]
   ^{:domain-label "permutations of a 4 item vector with one zero"}
   [(map vec (combo/permutations [0 5 -8 9])) 20 4]
   ^{:domain-label "permutations of a 4 item vector with two zeros"}
   [(map vec (combo/permutations [0 0 -8 9])) 10 2]
   ^{:domain-label "permutations of a 4 item vector with three zeros"}
   [(map vec (combo/permutations [0 0 0 9])) 4 0]
   ^{:domain-label "random cases"}
   [(fn [] (random-sequence-with-at-least-one-zero 5 44)) 78 974]
   ])

;;Can make Last Index of Zero test data like this:
;(test-and-train-data-from-domains last-index-of-zero-data-domains)

; Helper function for error function
(defn last-index-of-zero-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector % (.lastIndexOf % 0))
       inputs))

(def error-function 
  (gp/default-error-function :input-fn (fn [input] {:in1 input})
                             :out-stacks :integer
                             :error (fn [target output]
                                      (if (number? output)
                                        (math/abs (- output target)) ; distance from correct integer
                                        1000))))

(defn get-last-index-of-zero-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map last-index-of-zero-test-cases
       (utils/test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def data
  (get-last-index-of-zero-train-and-test last-index-of-zero-data-domains))

(defn last-index-of-zero-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn last-index-of-zero-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Last Index of Zero problem report - generation %s\n" generation)(flush)
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
     :max-initial-plushy-size 150
     :step-limit              600
     :restart 0.0
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.01
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
