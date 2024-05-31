;; mirror_image.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given two lists of integers of the same length <= 50, return true if one
;; list is the reverse of the other, and false otherwise.
;;
;; input stack has 2 input vectors of integers

(ns propeller.problems.software.mirror-image
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
            #?(:cljs [cljs.reader :refer [read-string]])))

; Atom generators
(def instructions
  (concat (list
            (fn [] (rand-nth (list true false))) ;Boolean
            ;;; end tag ERCs
            :in1
            :in2
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :vector_integer :exec})))


;; Define test cases
(defn mirror-image-input
  "Makes a Mirror Image input vector of length len."
  [len]
  (vec (repeatedly len
                   #(- (rand-int 2001) 1000))))

(defn change-a-few-elements
  "Takes a vector and changes from 1 to 5 elements in the vector."
  ([in-vec]
    (change-a-few-elements in-vec (inc (rand-int 5))))
  ([in-vec num-to-change]
    (if (>= 0 num-to-change)
      in-vec
      (change-a-few-elements (assoc in-vec (rand-int (count in-vec)) (- (rand-int 2001) 1000))
                             (dec num-to-change)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def mirror-image-data-domains
  [[(list [[] []]) 1 0] ;; Empty vectors
   [(list [[1] [1]]
          [[1] [0]]
          [[0] [1]]
          [[16] [-44]]
          [[-12] [-13]]) 5 0] ;; Length 1 vectors
   [(list [[1 2] [2 1]]
          [[1 1] [0 1]]
          [[7 0] [0 7]]
          [[5 8] [5 8]]
          [[34 12] [34 12]]
          [[456 456] [456 456]]
          [[-431 -680] [40 831]]) 7 0] ;; Length 2 vectors
   [(list [[1 2 1] [1 2 1]]
          [[1 2 3 4 5 4 3 2 1] [1 2 3 4 5 4 3 2 1]]
          [[45 99 0 12 44 7 7 44 12 0 99 45] [45 99 0 12 44 7 7 44 12 0 99 45]]
          [(vec (concat (reverse (range 25)) (range 25))) (vec (concat (reverse (range 25)) (range 25)))]) 4 0] ;; Equal Palindromes
   [(map #(vector [33 45 -941] (vec %))
         (combo/permutations [33 45 -941])) 6 0] ;; Permutations of a 3 item vector
   [(fn [] (let [inA (mirror-image-input (inc (rand-int 50)))]
             (vector inA (vec (reverse inA))))) 37 500] ;; true cases
   [(fn [] (let [inA (mirror-image-input (inc (rand-int 50)))]
             (vector inA inA))) 10 100] ;; equal vector cases
   [(fn [] (let [inA (mirror-image-input (inc (rand-int 50)))]
             (vector inA (change-a-few-elements (vec (reverse inA)))))) 20 200] ;; close calls cases (change a few elements at most)
   [(fn [] (let [inA (mirror-image-input (inc (rand-int 50)))]
             (vector inA (mirror-image-input (count inA))))) 10 200] ;; totally random cases
   ])

;;Can make Mirror Image test data like this:
;(test-and-train-data-from-domains mirror-image-data-domains)

; Helper function for error function
(defn mirror-image-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (= (first %) (vec (reverse (second %)))))
       inputs))

(defn get-mirror-image-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map mirror-image-test-cases
       (utils/test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def data
  (get-mirror-image-train-and-test mirror-image-data-domains))

(def error-function 
  (gp/default-error-function :out-stacks :boolean
                             :error (fn [target output]
                                      (if (= target output)
                                        0
                                        1))))

(defn mirror-image-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn mirror-image-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-with-test (error-function best :test)
        best-test-errors (:test-errors best-with-test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Mirror Image problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (doseq [[correct-output result] (map vector
                                         (map second (first data))
                                         (:behaviors best))]
      (println (format "Correct output: %5b | Program output: %s" correct-output (str result))))
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
     :max-generations         30
     :population-size         100
     :max-initial-plushy-size 150
     :step-limit              600
     :restart 0.0
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.01
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
