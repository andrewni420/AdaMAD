;; vector_average.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of floats with length in [1,50], with each float in [-1000,1000],
;; return the average of those floats. Results are rounded to 4 decimal places.
;;
;; input stack has 1 input vector of floats

(ns propeller.problems.software.vector-average
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
            [propeller.tools.gsemr :as gsemr]
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])
            [propeller.push.instructions :as instructions]))

; Atom generators
(def instructions
  (concat (list
            ;;; end constants
            ;;; end tag ERCs
            :in1
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:vector_float :float :integer :exec})))


;; Define test cases
(defn vector-average-input
  "Makes a Vector Average input vector of length len."
  [len]
  (vec (repeatedly len
                   #(- (* (rand) 2000.0) 1000.0))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def vector-average-data-domains
  [[(list [0.0] [100.0] [-100.0] [1000.0] [-1000.0]) 5 0] ;; Length-1 vectors
   [(fn [] (vector-average-input 1)) 45 500] ;; Random Length-1 vectors
   [(list [2.0 129.0]
          [0.12345 -4.678]
          [999.99 74.113]
          [987.654321 995.0003]
          [-788.788 -812.19]) 5 0] ;; Length-2 vectors
   [(fn [] (vector-average-input 2)) 45 500] ;; Random Length-2 vectors
   [(fn [] (vector-average-input (+ 3 (rand-int 3)))) 50 500] ;; Random Length-3, -4, and -5 vectors
   [(fn [] (vector-average-input 50)) 5 50] ;; Random Length-50 vectors
   [(fn [] (vector-average-input (inc (rand-int 50)))) 95 1000] ;; Random length, random floats
   ])

;;Can make Vector Average test data like this:
;(test-and-train-data-from-domains vector-average-data-domains)

; Helper function for error function
(defn vector-average-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (/ (apply + %)
                   (count %)))
       inputs))

(defn round-to-n-decimal-places
  "If a number, rounds float f to n decimal places."
  [f n]
  (if (not (number? f))
    f
    (let [factor (numeric/expt 10 n)]
      (double (/ (math/round (* f factor)) factor)))))

(def error-function 
  (gp/default-error-function :input-fn (fn [input] {:in1 input})
                             :out-stacks :float
                             :error (fn [target output]
                                      (round-to-n-decimal-places
                                       (if (number? output)
                                         (math/abs (- output target)) ; distance from correct integer
                                         1000000.0) ; penalty for no return value
                                       4))))

(defn get-vector-average-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map vector-average-test-cases
       (utils/test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def data
  (get-vector-average-train-and-test vector-average-data-domains))

(defn vector-average-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn vector-average-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Vector Average problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 1.0E-3)
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
     :max-initial-plushy-size 200
     :step-limit              800
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
  (maxbandit/gp
   (merge
    {#_:mapper #_mapv
     :instructions            instructions
     :error-function          error-function
     :training-data           (first data)
     :testing-data            (second data)
     :max-generations         300
     :population-size         [10000 1000]
     :max-initial-plushy-size 200
     :step-limit              800
     :parent-selection        :lexicase
     :tournament-size         5
     :update-freq 100
     :report-interval 1
     :transformations [maxbandit/tile-to-parameters-umad maxbandit/parameters-to-tile-umad]
     :bandit-parameters {:num-bandits 5
                         :l [-10]
                         :r [0]
                         :to [[0 0.03 0.06 0.09 0.12 0.15]] #_[[0 0.03 0.06 0.09 0.12 0.15 0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39 0.42 0.45 0.48]]
                         :ta [[0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39]] #_[[0.51 0.54 0.57 0.6 0.63 0.66 0.69 0.72 0.75 0.78 0.81 0.84 0.87 0.9]]
                         :num-codings 10
                         :d [1]
                         :acc [0.03]
                         :lr (into [] (map (fn [lr] {:method :momentum :lr (math/pow 10 (- (/ lr 9) 4)) :dampening 1 :momentum-lr 0.1})) (range 10))
                         #_[{:method :momentum
                             :momentum-lr 0.1
                             :lr 0.001
                             :dampening 1}]
                         :s [50]
                         :delta [0.01]
                         :n 300
                         :mode [:argmax]
                         :AO true
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 300 0.01)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                                                ;;  :dampening 1
                                                ;;  :momentum-lr [0.03]
                        ;;  :max-n (int-array (apply concat (map #(repeat % (* 10 %)) (range 30))))
                                                ;;  :legacy true
                         :max-n 100
                         :sigma 3}
     :reward-transformation :log-diff
    ;;  :parameter-override {:e 0.0001}
    ;;  :parameter-analysis [{:e 0.01} {:e 0.03} {:e 0.1} {:e 0.3} {:e 1} {:e 3}]
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))

