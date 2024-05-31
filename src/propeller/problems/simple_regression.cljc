(ns propeller.problems.simple-regression
  "Simple Regression:

Given inputs and outputs, find the target function."
  {:doc/format :markdown}
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])
            [propeller.tools.concurrent :as concurrent]))

(defn target-function
  "Target function: f(x) = x^3 + x + 3"
  [x]
  (+ (* x x x) x 3))


(def train-and-test-data
  "Training data: Inputs and outputs with -10 <= x < 11

  Test data: Inputs and outputs of -20 <= x < -10 and 11 <= x < 21"
  (let [train-inputs (range -10 11)
        test-inputs (concat (range -20 -10) (range 11 21))]
    {:train (map (fn [x] {:input1 (vector x) :output1 (vector (target-function x))}) train-inputs)
     :test (map (fn [x] {:input1 (vector x) :output1 (vector (target-function x))}) test-inputs)}))

(def instructions
  "stack-specific instructions, input instructions, close, and constants"
  (list :in1
        :integer_add
        :integer_subtract
        :integer_mult
        :integer_quot
        :integer_eq
        :exec_dup
        :exec_if
        'close
        0
        1))

(defn error-function
  "Finds the behaviors and errors of an individual. The error is the absolute
  deviation between the target output value and the program's selected behavior,
  or 1000000 if no behavior is produced. The behavior is here defined as the
  final top item on the INTEGER stack."
  ([argmap data individual]
   (let [program (genome/plushy->push (:plushy individual) argmap)
         inputs (map (fn [x] (first (:input1 x))) data)
         correct-outputs (map (fn [x] (first (:output1 x))) data)
         outputs (map (fn [input]
                        (state/peek-stack
                          (interpreter/interpret-program
                            program
                            (assoc state/empty-state :input {:in1 input})
                            (:step-limit argmap))
                          :integer))
                      inputs)
         errors (map (fn [correct-output output]
                       (if (= output :no-stack-item)
                         1000000
                         (math/abs (- correct-output output))))
                     correct-outputs
                     outputs)]
     (assoc individual
       :behaviors outputs
       :errors errors
       :total-error #?(:clj  (apply +' errors)
                       :cljs (apply + errors))))))



#_{:num-bandits 3
   :l [-5 -10]
   :r [5 0]
   :to [[0 0.1 0.2 0.3 0.4]
        [0 0.1 0.2 0.3 0.4]]
   :ta [[0.5 1 1.5]
        [0.5 1 1.5]]
   :num-codings 3
   :d [4]
   :acc [0.1 0.1]
   :lr [0.01 0.001 0.0001]
   :s [50]
   :delta [0.01]
   :n 100
   :mode [:argmax]
   :AO true
   :xi* [0.01]
   :epsilon [0.2]
   :maximize? true}

#_{:num-bandits 3
   :l [-5 -5 0 0 0 0 -5]
   :r [0  0  1 1 1 1 0]
   :to [[0 0.5 1]
        [0 0.5 1]
        [0 1/3]
        [0 1/3]
        [0 1/3]
        [0 1/3]
        [0 0.5 1]]
   :ta [[1 2]
        [1 2]
        [1/3 2/3]
        [1/3 2/3]
        [1/3 2/3]
        [1/3 2/3]
        [1 2]]
   :num-codings 3
   :d [4]
   :acc [0.5 0.5 1/3 1/3 1/3 1/3 0.5]
   :lr [0.01 0.001 0.0001]
   :s [50]
   :delta [0.01]
   :n 100
   :mode [:argmax]
   :AO true
   :xi* [0.01]
   :epsilon [0.2]
   :maximize? false}

#_{:num-bandits 3
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

(def basis (maxbandit/make-random-basis 3 21 :scale 10))

#_(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
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
       :transformations [#(maxbandit/tile-to-parameters-random % :basis basis :lexicase true) 
                         maxbandit/parameters-to-tile-random]
       :bandit-parameters {:num-bandits 3
                           :l [-1 -1 -1]
                           :r [1 1 1]
                           :to [[0 0.2 0.4]
                                [0 0.2 0.4]
                                [0 0.2 0.4]]
                           :ta [[0.4 0.6 0.8]
                                [0.4 0.6 0.8]
                                [0.4 0.6 0.8]]
                           :num-codings 3
                           :d [4]
                           :acc [0.2 0.2 0.2]
                           :lr [0.1]
                           :s [50]
                           :delta [0.01]
                           :n 100
                           :mode [:argmax]
                           :AO true
                           :xi* [0.01]
                           :epsilon [0.2]
                           :maximize? false}
       :parameter-override {:e 0.1}
       :custom-report maxbandit/custom-report
       :umad-rate                0.1
       :variation                {:umad 1}
       :mapper mapv
       :elitism                  false}
      (apply hash-map (map #(if (string? %) (read-string %) %) args)))))

#_(defn -main
  [& args]
  (gp/gp
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

;5000 generations x 5 popsize
;umad 0.1 = 79.3%
;maxbandit log(r) 10000n 0.1lr = 79.3%
;maxbandit 1/r 10000n 0.1lr = 72.4%
;maxbandit 1/r 10000n 0.1lr [-5 -2]= 72.4%
;maxbandit 1/r 10000n [-5 -2] = 70%
;maxbandit 1/r 10000n 0.2lr = 65.5%
;maxbandit 1/r 100e 10000n = 64.0%
;maxbandit 1/r = 62.1% 
;maxbandit 1/r 1000e = 62.1% 
;maxbandit 1/r 10000n 0.2lr [-5 -2] = 58.6%
;maxbandit 100e = 52.0%
;umad 0.01 = 51.7%
;maxbandit sigmoid = 40.0%
;maxbandit = 33.5% 
;maxbandit 1000e = 24.1% 

;10000 generations x 5 popsize
;maxbandit log = 62.0% = 0.968E-4 log/gens
;multirun 10 epsilon-greedy 0.2 = 100.0% = ##Inf log/gen
;umad0.1 = 86.0% = 1.97E-4 log/gen

;5000 generations x 5 popsize 
;maxbandit log = 74.0% = 2.69E-4 log/gens
;multirun 10 epsilon-greedy 0.2 = 100.0% = ##Inf log/gen
;umad0.1 = 82.0% = 3.43E-4 log/gens

;4000 generations x 5 popsize 
;maxbandit log = 48.0% = 1.63E-4 log/gens
;multirun 10 epsilon-greedy 0.2 = 84.0% = 4.58E-4 log/gen
;umad0.1 = 84.0% = 4.58E-4 log/gens

;3000 generations x 5 popsize
;maxbandit log = 50.0% = 2.31E-4 log/gens
;multirun 10 epsilon-greedy 0.2 = 80.0% = 5.36E-4 log/gen
;umad0.1 = 76.0% = 4.76E-4 log/gens

;2000 generations x 5 popsize
;maxbandit log = 52.0% = 3.67E-4 log/gens
;multirun 10 epsilon-greedy 0.2 = 78.0% = 7.57E-4 log/gen
;umad0.1 = 52.0% = 3.67E-4 log/gens

;1000 generations x 5 popsize 
;maxbandit log = 26% = 3.011E-4 log/gen
;multirun 10 epsilon-greedy 0.2 = 52.0% = 7.34E-4 log/gen
;umad0.1 = 46.0% = 6.16E-4 log/gen

;500 generations x 5 popsize
;maxbandit log = 10.0% = 2.11E-4 log/gen
;multirun 10 epsilon-greedy 0.2 = 22.0% = 4.97E-4 log/gen
;umad0.1 = 26.0% = 6.02E-4 log/gen

;200 generations x 5 popsize
;maxbandit log = 2.0% = 1.01E-4 log/gen
;multirun 10 epsilon-greedy 0.2 = 4.0% = 2.04E-4 log/gen
;umad0.1 = 6.0% = 3.09E-4 log/gen

;100 generations x 5 popsize
;maxbandit log = 0% = 0 log/gen
;multirun 10 epsilon-greedy 0.2 = 0% = 0 log/gen
;umad0.1 = 8.0% = 8.34E-4 log/gen


#_(let [p (/ 41 50.)
      gen 5000]
  (println "p " p)
  (println "log " (/ (- (Math/log (- 1 p))) gen)))

#_{:num-bandits 3
   :l [-5 0]
   :r [5 1]
   :to [[0 0.1 0.2 0.3 0.4]
        [0 0.01 0.02 0.03 0.04]]
   :ta [[0.5 0.7 1]
        [0.05 0.07 0.1]]
   :num-codings 3
   :d [4]
   :acc [0.1 0.01]
   :lr [0.999 0.9999 0.99999]
   :s [5]
   :delta [0.01]
   :n 100;timescale of the changing of the maxbandit distribution
   :mode [:argmax :sample]
   :maximize? false}





