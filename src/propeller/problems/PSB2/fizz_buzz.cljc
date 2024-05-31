(ns propeller.problems.PSB2.fizz-buzz
  "FIZZ BUZZ from PSB2
Given an integer x, return \"Fizz\" if x is
divisible by 3, \"Buzz\" if x is divisible by 5, \"FizzBuzz\" if x
is divisible by 3 and 5, and a string version of x if none of
the above hold.

Source: https://arxiv.org/pdf/2106.06086.pdf"
  {:doc/format :markdown}
  (:require [psb2.core :as psb2]
            [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.utils :as utils]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.push.state :as state]
            [propeller.tools.metrics :as metrics]
            [propeller.gp :as gp]
            [propeller.tools.gsemr :as gsemr]
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])))

(def train-and-test-data "Data taken from https://zenodo.org/record/5084812" (psb2/fetch-examples "data" "fizz-buzz" 200 2000))

(def instructions
  "Stack-specific instructions, input instructions, close, and constants"
  (utils/not-lazy
    (concat
      ;;; stack-specific instructions
      (get-stack-instructions #{:exec :integer :boolean :string :print})
      ;;; input instructions
      (list :in1)
      ;;; close
      (list 'close)
      ;;; ERCs (constants)
      (list "Fizz" "Buzz" "FizzBuzz" 0 3 5))))

(defn error-function
  "Finds the behaviors and errors of an individual: Error is 0 if the value and
  the program's selected behavior match, or 1 if they differ, or 1000000 if no
  behavior is produced. The behavior is here defined as the final top item on
  the STRING stack."
  [argmap data individual]
   (let [program (genome/plushy->push (:plushy individual) argmap)
         inputs (map (fn [i] (get i :input1)) data)
         correct-outputs (map (fn [i] (get i :output1)) data)
         outputs (map (fn [input]
                        (state/peek-stack
                          (interpreter/interpret-program
                            program
                            (assoc state/empty-state :input {:in1 input})
                            (:step-limit argmap))
                          :string))
                      inputs)
         errors (map (fn [correct-output output]
                       (if (= output :no-stack-item)
                         10000
                         (metrics/levenshtein-distance correct-output output)))
                     correct-outputs
                     outputs)]
     (assoc individual
       :behaviors outputs
       :errors errors
       :total-error #?(:clj  (apply +' errors)
                       :cljs (apply + errors)))))


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
       :max-initial-plushy-size 250
       :step-limit              2000
       :parent-selection        :lexicase
       :tournament-size         5
       :umad-rate               0.05
       :restart                 0
       :variation               {:umad 1.0 :crossover 0.0}
       :elitism                 false}
      (apply hash-map (map #(if (string? %) (read-string %) %) args)))))


(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (gsemr/gp
   (merge
    {#_:mapper #_mapv
     :instructions            instructions
     :error-function          error-function
     :training-data           (:train train-and-test-data)
     :testing-data            (:test train-and-test-data)
     :max-generations         300
     :population-size         [1000 1000]
     :max-initial-plushy-size 250
     :step-limit              2000
     :parent-selection        :lexicase
     :tournament-size         5
     :update-freq 100
     :report-interval 1
     :transformations [maxbandit/tile-to-parameters-umad maxbandit/parameters-to-tile-umad]
     :bandit-parameters {:num-bandits 1
                         :l [-10]
                         :r [0]
                         :to [[0 0.03 0.06 0.09 0.12 0.15]] #_[[0 0.03 0.06 0.09 0.12 0.15 0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39 0.42 0.45 0.48]]
                         :ta [[0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39]] #_[[0.51 0.54 0.57 0.6 0.63 0.66 0.69 0.72 0.75 0.78 0.81 0.84 0.87 0.9]]
                         :num-codings 20
                         :d [1]
                         :acc [0.03]
                         :lr [{:method :momentum
                               :momentum-lr 0.1
                               :lr 0.001
                              ;;  :update-freq (int-array (apply concat (map #(repeat % (* 10 %)) (range 30))))
                               :dampening 1
                              ;;  :alpha 0.0003
                              ;;  :beta1 0.03
                              ;;  :beta2 0.001
                              ;;  :warm-start 5
                               }]
                         :s [50]
                         :delta [0.01]
                         :n 300
                         :mode [:argmax]
                         :AO true
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                                                ;;  :dampening 1
                                                ;;  :momentum-lr [0.03]
                        ;;  :max-n (int-array (apply concat (map #(repeat % (* 10 %)) (range 30))))
                                                ;;  :legacy true
                         :max-n 100
                         :sigma 3}
     :reward-transformation :log-diff
    ;;  :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.01} {:e 0.03} {:e 0.1} {:e 0.3} {:e 1} {:e 3}]
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))

