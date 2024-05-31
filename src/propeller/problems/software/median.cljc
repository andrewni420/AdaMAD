;; median.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given 3 integers, print their median.
;;
;; input stack has the 3 integers

(ns propeller.problems.software.median
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.metrics :as metrics]
            [clojure.string :as string]
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])
            [propeller.push.instructions :as instructions]))

; Atom generators
(def instructions
  (concat (list
           (fn [] (- (rand-int 201) 100))
            ;;; end ERCs
           :in1
           :in2
           :in3
           'close
            ;;; end input instructions
           )
          (get-stack-instructions #{:integer :boolean :exec :print})))

;; A list of data domains for the median problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def median-data-domains
  [#_[(list [2 6 8] [2 8 6] [6 2 8] [6 8 2] [8 2 6] [8 6 2] ; Permutations of [2 6 8]
           [-5 0 5] [-5 5 0] [0 -5 5] [0 5 -5] [5 -5 0] [5 0 -5] ; Permutations of [-5 0 5]
           [23 0 0] [0 23 0] [0 0 23] [-31 0 0] [0 -31 0] [0 0 -31]) 18 0] ; Two zeroes
   [(fn [] (repeatedly 3 #(- (rand-int 201) 100))) 60 600] ;; Each input includes 3 integers in range [-100,100]
   [(fn [] (shuffle (conj (repeat 2 (- (rand-int 201) 100))
                          (- (rand-int 201) 100)))) 30 300] ;; Edge cases where two of three are the same
   [(fn [] (repeat 3 (- (rand-int 201) 100))) 10 100] ;; Edge cases where all are the same
   ])

;;Can make median test data like this:
;(test-and-train-data-from-domains median-data-domains)

; Helper function for error function
(defn median-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2 input3] output]."
  [inputs]
  (map #(vector %
                (second (sort %)))
       inputs))

(def error-function 
  (gp/default-error-function :initialized-stacks {:print '("")}
                             :error (fn [target output]
                                      (if (= output (str target))
                                        0
                                        1))))

(defn get-median-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map median-test-cases
       (utils/test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def data
  (get-median-train-and-test median-data-domains))


(defn median-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn median-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Median problem report - generation %s\n" generation)(flush)
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
     :max-initial-plushy-size 100
     :step-limit              200
     :restart 0.0
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))

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
     :num-runs 50
     :max-generations         10000
     :population-size         [1000 5]
     :max-initial-plushy-size 100
     :step-limit              200
     :restart 0.0
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))



#_(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (maxbandit/gp
   (merge
    {:instructions             instructions
     :error-function           error-function
     :training-data            (first data)
     :testing-data             (second data)
     :max-generations          5000
     :population-size          5
     :max-initial-plushy-size 100
     :step-limit              200
     :parent-selection         :lexicase
     :tournament-size          5
     :n 10000;;how many individuals to keep
     :transformations [maxbandit/tile-to-parameters-umad maxbandit/parameters-to-tile-umad]
     :bandit-parameters {:num-bandits 3
                         :l [-10]
                         :r [0]
                         :to [[0 0.1 0.2 0.3 0.4]]
                         :ta [[0.5 0.7 1]]
                         :num-codings 3
                         :d [4]
                         :acc [0.1]
                         :lr [0.1]
                         :s [50]
                         :delta [0.01]
                         :n 10000
                         :mode [:argmax]
                         :AO true
                         :xi* [0.01]
                         :epsilon [0.2]
                         :maximize? false}
     :parameter-override {:e 0.1}
     :custom-report maxbandit/custom-report
     :umad-rate                0.1
     :mapper mapv
     :variation                {:umad 1}
     :elitism                  false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))


;;lexicase = 97 solutions, 48 generalizing solutions
;;weighted lexicase 10 = 89 solutions, 46 generalizing solutions
;;weighted lexicase 20 = 88 solutions, 47 generalizing solutions
;;weighted lexicase 100 = 93 solutions, 60 generalizing solutions
;;weighted lexicase 200 = 98 solutions, 48 generalizing solutions
