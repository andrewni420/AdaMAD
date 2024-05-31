(ns propeller.problems.software.number-io
  "Number IO from iJava (http://ijava.cs.umass.edu/)

     This problem file defines the following problem:
There are two inputs, a float and an int. The program must read them in, find
their sum as a float, and print the result as a float.
     "
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])))

;; =============================================================================
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; NUMBER IO PROBLEM
;;
;; This problem file defines the following problem:
;; There are two inputs, a float and an int. The program must read them in, find
;; their sum as a float, and print the result as a float.
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; NOTE: input stack: in1 (float),
;;                    in2 (int)
;;       output stack: printed output
;; =============================================================================

;; =============================================================================
;; DATA DOMAINS
;;
;; A list of data domains. Each domain is a map containing a "set" of inputs
;; and two integers representing how many cases from the set should be used as
;; training and testing cases respectively. Each "set" of inputs is either a
;; list or a function that, when called, will create a random element of the set
;; =============================================================================

;; Random float between -100.0 and 100.0
(defn random-float "Random float between -100.0 and 100.0" [] (- (* (rand) 200) 100.0))

; Random integer between -100 and 100
(defn random-int "Random integer between -100 and 100" [] (- (rand-int 201) 100))

(def instructions
  "Stack-specific instructions, input instructions, close, and constants"
  (utils/not-lazy
    (concat
      ;; stack-specific instructions
      (get-stack-instructions #{:float :integer :print})
      ;; input instructions
      (list :in1 :in2 'close)
      ;; ERCs (constants)
      (list random-float random-int))))

(def train-and-test-data
  "Inputs are random integers and random floats and outputs are the sum as a float."
  (let [inputs (vec (repeatedly 1025 #(vector (random-int) (random-float))))
        outputs (mapv #(apply + %) inputs)
        train-set (mapv vector (take 25 inputs) (take 25 outputs))
        test-set (mapv vector (drop 25 inputs) (drop 25 outputs))]
    {:train train-set
     :test  test-set}))


#_(defn error-function
  "Finds the behaviors and errors of an individual: Error is the absolute difference between
  program output and the correct output.
  The behavior is here defined as the final top item on
  the PRINT stack."
  [argmap data individual]
  (let [program (genome/plushy->push (:plushy individual) argmap)
        inputs (:inputs data)
        correct-outputs (:outputs data)
        outputs (map (fn [input]
                       (state/peek-stack
                         (interpreter/interpret-program
                           program
                           (assoc state/empty-state :input {:in1 (first input)
                                                            :in2 (last input)}
                                                    :print '(""))
                           (:step-limit argmap))
                         :print))
                     inputs)
        parsed-outputs (map (fn [output]
                              (try (read-string output)
                                   #?(:clj  (catch Exception e 1000.0)
                                      :cljs (catch js/Error. e 1000.0))))
                            outputs)
        errors (map (fn [correct-output output]
                      (min 1000.0 (math/abs (- correct-output output))))
                    correct-outputs
                    parsed-outputs)]
    (assoc individual
      :behaviors parsed-outputs
      :errors errors
      :total-error #?(:clj  (apply +' errors)
                      :cljs (apply + errors)))))

#_(def proxy-error 
  (gp/proxy-duplicate #{0 1 2} 1 25))

(def proxy-error 
  (gp/proxy-weighted 25 :normal 25 1))

#_(def proxy-error 
  (gp/proxy-agg ))


(def error-function
  (gp/default-error-function :initialized-stacks {:print '("")}
                             :error (fn [target output]
                                      (min 1000.0 (math/abs (- target (try (read-string output)
                                                                           #?(:clj  (catch Exception _ 1000.0)
                                                                                      :cljs (catch js/Error. _ 1000.0)))))))
                             :proxy-error nil #_proxy-error))

#_(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (gp/gp
   (merge
    {:instructions            instructions
     :error-function          error-function
     :training-data           (:train train-and-test-data)
     :testing-data            (:test train-and-test-data)
     :max-generations         300
     :population-size         1000
     :max-initial-plushy-size 100
     :step-limit              200
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.1
     :variation               {:umad 1}
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
     :training-data           (:train train-and-test-data)
     :testing-data            (:test train-and-test-data)
     :max-generations         300
     :population-size         [1000 100]
     :max-initial-plushy-size 100
     :step-limit              200
     :parent-selection        :lexicase
     :tournament-size         5
     :update-freq 100
     :report-interval 10
     :transformations [maxbandit/tile-to-parameters-umad maxbandit/parameters-to-tile-umad]
     :bandit-parameters {:num-bandits 1
                         :l [-10]
                         :r [0]
                         :to [[0 0.06 0.12]]
                         :ta [[0.15 0.21 0.27]]
                         :num-codings 3
                         :d [5]
                         :acc [0.03]
                         :lr [0.01]
                         :s [50]
                         :delta [0.01]
                         :n 100
                         :mode [:softmax]
                         :temperature [50]
                         :AO true
                         :xi* [0.01]
                         :epsilon [0.2]
                         :maximize? false}
    ;;  :parameter-override {:e 0.1}
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))


;25000 generations x 5 popsize
;umad 0.1 = 78.0% = 0.606E-4 log/gen
;maxbandit 1/r 10000n = 70.0% = 0.482E-4 log/gen
;t1, umad0.1 = 64.0% = 0.409E-4 log/gen
;t0.01, umad0.1 = 58.0% = 0.347E-4 log/gen
;maxbandit 0.1lr = 58.0% = 0.347E-4 log/gen
;umad 0.01 = 20.0% = 0.089E-4 log/gen

;10000 generations x 5 popsize
;umad 0.1 = 78.0% = 1.514E-4 log/gen
;multirun 10 epsilon-greedy 0.2 = 100.0% = ##Inf log/gen

;5000 generations x 5 popsize
;umad 0.1 = 66.0% = 2.158E-4 log/gen
;umad 0.1 = 74% = 2.694E-4 log/gen
;t0.01 umad0.1 = 37.9% = 0.953E-4 log/gen
;t0.1 umad0.1 = 41.4% = 1.069E-4 log/gen
;t1 umad0.1 = 48.3% = 1.319E-4 log/gen
;multirun 10 epsilon-greedy 0.2 = 94.0% = 5.63E-4 log/gen

;4000 generations x 5 popsize
;multirun 10 epsilon-greedy 0.2 = 86.0% = 4.92E-4 log/gen

;3000 generations x 5 popsize
;multirun 10 epsilon-greedy 0.2 = 78.0% = 5.05E-4 log/gen

;2000 generations x 5 popsize
;multirun 10 epsilon-greedy 0.2 = 74.0% = 6.74E-4 log/gen

;1000 generations x 5 popsize
;umad 0.1 = 48.0% = 6.539E-4 log/gen
;multirun 10 epsilon-greedy 0.2 = 54.0% = 7.77E-4 log/gen

;500 generations x 5 popsize
;umad0.1 = 46.0% = 12.323E-4 log/gen
;multirun 10 epsilon-greedy 0.2 = 22.0% = 4.97E-4 log/gen

;200 generations x 5 popsize
;umad 0.1 = 18.0% = 9.923E-4 log/gen
;multirun 10 epsilon-greedy 0.2 = 14.0% = 7.54E-4 log/gen

;100 generations x 5 popsize
;umad 0.1 = 6.0% = 6.188E-4 log/gen
;multirun 10 epsilon-greedy 0.2 = 4.0% = 4.08E-4 log/gen

#_(let [p (/ 43 50.)
      gen 4000]
  (println "p " p)
  (println "log " (/ (- (Math/log (- 1 p))) gen)))

;lexicase: 100 successes, 87 generalizing 
;weighted lexicase 20: 100 successes, 95 generalizing
;weighted lexicase 20 [-5 0] uniform random exponent: 100 successes, 90 generalizing
;weighted lexicase uniform 1: 100 successes, 88 generalizing 
;weighted lexicase uniform 5: 100 successes, 93 generalizing
;weighted lexicase uniform 20: 100 successes, 86 generalizing
;weighted lexicase uniform 100: 100 successes, 89 generalizing
;plexicase myimpl 0.5: 100 successes, 97 generalizing
;plexicase myimpl 1: 100 successes, 93 generalizing
;plexicase myimpl 2: 100 successes, 85 generalizing
;weighted lexicase range 1: 93 successes, 82 generalizing 
;weighted lexicase range 5: 97 successes, 90 generalizing 
;weighted lexicase range 20: 99 successes, 90 generalizing
;weighted lexicase range 100: 100 successes, 90 generalizing 

;downsampled 0.1: 100 successes, 100 generalizing 
;downsampled 0.25: 100 successes, 100 generalizing
;downsampled 0.25 inherit 4: 100 successes, 100 generalizing 

