;; for_loop_index.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given 3 integer inputs (start, finish, stepSize), print the integers
;; represented by the Java for loop:
;;      for(i = start; i < finish; i += stepSize) System.out.println(i);
;;
;; Note that start < finish for all test cases, so will always require printing something.
;;
;; Note: tried adding extra error, did not help
;;
;; input stack has 3 input integers: in1 = start, in2 = finish, in3 = step-size

(ns propeller.problems.software.for-loop-index
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
            ;;; end tag ERCs
            :in1
            :in2
            :in3
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :exec :print})))


;; Define test cases
(defn loop-input
  "Makes a For Loop Index input vector = [start, finish, step-size] with start < finish."
  [& {:keys [neg-pos] :or {neg-pos false}}]
  (if (not neg-pos)
    (let [step-size (inc (rand-int 10))
          start (- (rand-int 1000) 500)
          finish (+ start 1 (rand-int (* 20 step-size)))]
      [start finish step-size])
    (let [step-size (inc (rand-int 10))
          start (dec (- (rand-int (* 10 step-size))))
          finish (rand-int (* 10 step-size))]
      [start finish step-size])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def loop-data-domains
  [[(fn [] (loop-input :neg-pos true)) 10 100] ;; Cases where start < 0 and finish > 0
   [(fn [] (loop-input)) 90 900] ;; Random cases
   ])

;;Can make For Loop Index test data like this:
;(test-and-train-data-from-domains loop-data-domains)

; Helper function for error function
(defn loop-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (apply str (interpose \newline (apply range %))))
       inputs))

(def error-function 
  (gp/default-error-function :initialized-stacks {:print '("")}))


(defn get-for-loop-index-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map loop-test-cases
       (utils/test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def data
  (get-for-loop-index-train-and-test loop-data-domains))


(defn for-loop-index-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn loop-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- For Loop Index problem report - generation %s\n" generation)(flush)
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
