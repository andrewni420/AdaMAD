;; digits.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given an integer |x| < 10,000,000,000, print that integer's digits each on
;; their own line starting with the least significant digit. A negative integer
;; should have the negative sign printed before the most significant digit.
;;
;; input stack has input integer

(ns propeller.problems.software.digits
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
            #?(:cljs [cljs.reader :refer [read-string]])))

; Atom generators
(def instructions
  (concat (list
            \newline
            ;;; end constants
            (fn [] (- (rand-int 21) 10))
            ;;; end tag ERCs
            :in1
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :string :char :exec :print})))

(defn my-rand-long
  "replaces rand-int when need longs"
  [end]
  (long (* (rand) end)))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def digits-data-domains
  [[(list -9495969798 -20008000 -777777 -9876 -482 -97 -20 0 19 620 24068 512000 8313227 30000000 9998887776) 15 0] ;; Edge cases by hand
   [(fn []
      (let [digs (inc (rand-int 10))
            start (numeric/expt 10 (dec digs))
            end (numeric/expt 10 digs)]
        ((if (< (rand) 0.5) - +)
          (+ (my-rand-long (- end start)) start)))) 85 1000] ;; Random cases such that each number of digits between 1 and 10 will be represented evenly, as will negatives and positives
   ])

;;Can make Digits test data like this:
;(test-and-train-data-from-domains digits-data-domains)

; Helper function for error function
(defn digits-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in] (vector in
                        (apply str ((if (< in 0) #(concat (butlast %) [\- (last %)]) identity)
                                     (interpose \newline (reverse (str (math/abs in))))))))
       inputs))

(def error-function 
  (gp/default-error-function :input-fn (fn [input] {:in1 input})
                             :initialized-stacks {:print '("")}))

(defn get-digits-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map digits-test-cases
                 (utils/test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def data
  (get-digits-train-and-test digits-data-domains))

(defn digits-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn digits-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Digits problem report - generation %s\n" generation)(flush)
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

