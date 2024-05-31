(ns propeller.problems.PSB2.substitution-cipher
  "SUBSTITUTION CIPHER from PSB2

This problem gives 3 strings.
The first two represent a cipher, mapping each character in
one string to the one at the same index in the other string.
The program must apply this cipher to the third string and
return the deciphered message.

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
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])))

(def train-and-test-data "Data taken from https://zenodo.org/record/5084812" (psb2/fetch-examples "data" "substitution-cipher" 200 2000))

(defn map-vals-input
  "Returns all the input values of a map"
  [i]
  (vals (select-keys i [:input1 :input2 :input3])))

(defn map-vals-output
  "Returns the output values of a map"
  [i]
  (vals (select-keys i [:output1])))

(def instructions
  "Stack-specific instructions, input instructions, close, and constants"
  (utils/not-lazy
   (concat
      ;;; stack-specific instructions
    (get-stack-instructions #{:exec :integer :boolean :char :string :print})
      ;;; input instructions
    (list :in1 :in2 :in3)
      ;;; close
    (list 'close)
      ;;; ERCs (constants)
    (list 0 ""))))

(def train-data
  (map #(vector (map-vals-input %) (first (map-vals-output %))) (:train train-and-test-data)))

(def test-data
  (map #(vector (map-vals-input %) (first (map-vals-output %))) (:test train-and-test-data)))


(def error-function
  (gp/default-error-function :out-stacks :string
                             :error (fn [correct-output output]
                                      (if (= output :no-stack-item)
                                        10000
                                        (metrics/levenshtein-distance correct-output output)))))

(defn -main
    "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
    [& args]
    (gp/gp
     (merge
      {:instructions            instructions
       :error-function          error-function
       :training-data           train-data
       :testing-data            test-data
       :max-generations         300
       :population-size         1000
       :max-initial-plushy-size 250
       :step-limit              2000
       :parent-selection        :lexicase
       :tournament-size         5
       :umad-rate               0.1
       :variation               {:umad 1.0 :crossover 0.0}
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
     :training-data           train-data
     :testing-data            test-data
     :max-generations         300
     :population-size         [1000 1000]
     :max-initial-plushy-size 100
     :step-limit              200
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
                               :dampening 1
                              ;;  :alpha 0.0003
                              ;;  :beta1 0.03
                              ;;  :beta2 0.001
                              ;;  :warm-start 5
                               }]
                         :s [50]
                         :delta [0.01]
                         :n 300
                         :mode [:softmax-scaled]
                         :AO true
                         :xi* [0.01]
                         :epsilon [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                        ;;  :dampening 1
                        ;;  :momentum-lr [0.03]
                         :max-n 100
                        ;;  :legacy true
                         :sigma 3}
     :generations-per-update 1
     :reward-transformation :log-diff
     :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.01} {:e 0.03} {:e 0.1} {:e 0.3} {:e 1} {:e 3}]
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))


