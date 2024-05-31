;; sum_of_squares.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given integer 0 < n <= 100, return the sum of squaring each positive integer
;; between 1 and n inclusive.
;;
;; input stack has integer n

(ns propeller.problems.software.sum-of-squares
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
            0
            1
            ;;; end constants
            (fn [] (- (rand-int 201) 100)) ;Integer ERC [-100,100]
            ;;; end tag ERCs
            :in1
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :exec})))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def sum-of-squares-data-domains
  [[(range 1 6) 5 0] ; Small cases
   [(list 100) 1 0] ; Last case
   [(fn [] (+ 6 (rand-int 94))) 44 0] ; Random cases [6,99]
   [(range 1 101) 0 100] ; Test all integers in [1,100]
   ])

;;Can make Sum Of Squares test data like this:
;(test-and-train-data-from-domains sum-of-squares-data-domains)

; Helper function for error function
(defn sum-of-squares-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (apply +' (map #(*' % %) (range (inc in))))))
       inputs))

(def error-function 
  (gp/default-error-function :input-fn (fn [input] {:in1 input})
                             :out-stacks :integer
                             :error (fn [target output]
                                      (if (number? output)
                                        (math/abs (- target output)) ;distance from correct integer
                                        1000000000))))

(defn get-sum-of-squares-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map sum-of-squares-test-cases
                 (utils/test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def data
  (get-sum-of-squares-train-and-test sum-of-squares-data-domains))

(defn sum-of-squares-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn sum-of-squares-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Sum Of Squares problem report - generation %s\n" generation)(flush)
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
     :max-initial-plushy-size 200
     :step-limit              4000
     :restart 0.0
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.01
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
