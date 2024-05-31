(ns propeller.problems.software.smallest
  "SMALLEST PROBLEM from C. Le Goues et al.,
  \"The ManyBugs and IntroClass Benchmarks\n
   for Automated Repair of C Programs,\" in IEEE Transactions on Software\n
    Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.\n
     doi: 10.1109/TSE.2015.2454513

This problem file defines the following problem:
takes as input four ints, computes the smallest, and prints to the screen the smallest input."
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.gp :as gp]
            [propeller.tools.concurrent :as concurrent]
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])))

;; =============================================================================
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; SMALLEST PROBLEM
;;
;; Problem Source: C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks
;; for Automated Repair of C Programs," in IEEE Transactions on Software
;; Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;; doi: 10.1109/TSE.2015.2454513
;; =============================================================================

;; =============================================================================
;; DATA DOMAINS
;;
;; A list of data domains. Each domain is a map containing a "set" of inputs
;; and two integers representing how many cases from the set should be used as
;; training and testing cases respectively. Each "set" of inputs is either a
;; list or a function that, when called, will create a random element of the set
;; =============================================================================

; Random integer between -100 and 100
(defn random-int "Random integer between -100 and 100" [] (- (rand-int 201) 100))

(def instructions
  "Stack-specific instructions, input instructions, close, and constants"
  (utils/not-lazy
    (concat
      ;; stack-specific instructions
      (get-stack-instructions #{:boolean :exec :integer :print})
      ;; input instructions
      (list :in1 :in2 :in3 :in4 'close)
      ;; ERCs (constants)
      (list random-int))))

(def train-and-test-data
  "Inputs are 4-tuples of random integers and the outputs are the minimum value of each tuple."
  (let [inputs (vec (repeatedly 1100 #(vector (random-int) (random-int)
                                              (random-int) (random-int))))
        outputs (mapv #(apply min %) inputs)
        train-set (map vector (take 100 inputs) (take 100 outputs))
        test-set (map vector (drop 100 inputs) (drop 100 outputs))]
    {:train train-set
     :test  test-set}))

(def error-function 
  (gp/default-error-function :initialized-stacks {:print '("")}
                             :error (fn [target output]
                                      (if (= (str target) (str output)) 0 1))))


#_(defn -main
  " Runs the top-level genetic programming function, giving it a map of
arguments with defaults that can be overridden from the command line
or through a passed map. "
  [& args]
  (maxbandit/gp
    (merge
      {:instructions             instructions
       :error-function           error-function
       :training-data            (:train train-and-test-data)
       :testing-data             (:test train-and-test-data)
       :max-generations          5000
       :population-size          5
       :max-initial-plushy-size  100
       :step-limit               200
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


#_(defn -main
    [& args]
    (gp/max-gp
     (merge
      {:instructions             instructions
       :error-function           error-function
       :training-data            (:train train-and-test-data)
       :testing-data             (:test train-and-test-data)
       :max-generations          100
       :num-runs 10
       :report-interval 100
       :num-experiments 1
       :population-size          [10000 5]
       :max-initial-plushy-size  100
       :step-limit               200
       :parent-selection         :lexicase
       :tournament-size          5
       :epsilon 0.2
       :sampling-type :softmax-ranked
       :umad-rate                0.1
       :mapper mapv
       :variation                {:umad 1}
       :elitism                  false}
      (apply hash-map (map #(if (string? %) (read-string %) %) args)))))


(defn -main
    [& args]
    (gp/gp
     (merge
      {:instructions             instructions
       :error-function           error-function
       :training-data            (:train train-and-test-data)
       :testing-data             (:test train-and-test-data)
       :max-generations          300
       :population-size          1000
       :max-initial-plushy-size  100
       :step-limit               200
       :parent-selection         :lexicase
       :lexicase-scale 10
       :tournament-size          5
       :umad-rate                0.1
       :variation                {:umad 1}
       :elitism                  false}
      (apply hash-map (map #(if (string? %) (read-string %) %) args)))))

;service-mapper2 1:19
;agent-mapper 1:19
;service-mapper3 1:43
;service-mapper 3:29
;updated service-mapper 1:26
;service-mapper-manual 6:31
;future-mapper 1:09

;lexicase = 100/100 solutions, 81/83 generalizing
;weighted 10 = 100 solutions, 82 generalizing 
;weighted 20 = 100 solutions, 90 generalizing 
;weighted 100 = 100 solutions, 88 generalizing
;weighted 200 = 100 solutions, 86 generalizing
;weighted 300 = 72 solutions, 56 generalizing
;weighted 500 = 0 solutions, 0 generalizing

;weighted uniform5 = 100 solutions, 89 generalizing 
;weighted uniform20 (OOM) = 87 solutions, 73 generalizing
;weighted uniform100 = 100 solutions, 85 generalizing

;weighted range5 = 100 solutions, 90 generalizing 
;weighted range20 = 100 solutions, 86 generalizing 
;weighted range100 = 100 solutions, 80 generalizing 

;plexicase 2 = 100 solutions, 94 generalizing 



