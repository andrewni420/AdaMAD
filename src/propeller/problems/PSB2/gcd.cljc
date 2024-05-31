(ns propeller.problems.PSB2.gcd
  "GCD [GREATEST COMMON DIVISOR] from PSB2

Given two integers, return the largest integer that divides each
of the integers evenly

Source: https://arxiv.org/pdf/2106.06086.pdf"
  {:doc/format :markdown}
  (:require [psb2.core :as psb2]
            [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.utils :as utils]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.push.state :as state]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.gesmr :as gesmr]
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])))

(def train-and-test-data "Data taken from https://zenodo.org/record/5084812" (psb2/fetch-examples "data" "gcd" 200 2000))

(defn random-int "Random integer between -100 and 100" [] (- (rand-int 201) 100))

(defn map-vals-input
  "Returns all the input values of a map"
  [i]
  (vals (select-keys i [:input1 :input2])))

(defn map-vals-output
  "Returns the output values of a map"
  [i]
  (get i :output1))

(def instructions
  "Stack-specific instructions, input instructions, close, and constants"
  (utils/not-lazy
    (concat
      ;;; stack-specific instructions
      (get-stack-instructions #{:exec :integer :boolean :print})
      ;;; input instructions
      (list :in1 :in2)
      ;;; close
      (list 'close)
      ;;; ERCs (constants)
      (list random-int))))


(def train-data
  (map #(vector (map-vals-input %) (map-vals-output %)) (:train train-and-test-data)))

(def test-data
  (map #(vector (map-vals-input %) (map-vals-output %)) (:test train-and-test-data)))


(def error-function
  (gp/default-error-function :out-stacks :integer
                             :error (fn [correct-output output]
                                      (if (= output :no-stack-item)
                                        1000000.0
                                        (math/abs (- correct-output output))))))


(def argmap 
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
   :elitism                 false})

#_(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (apply
   gp/run-experiment
   100
   argmap
   (map #(if (string? %) (read-string %) %) args)))

#_(defn -main
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
  (gesmr/gp
   (merge
    {#_:mapper #_mapv
     :instructions            instructions
     :error-function          error-function
     :training-data           train-data
     :testing-data            test-data
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
                         :d [5]
                         :acc [0.03]
                         :lr [0.001]
                         :s [50]
                         :delta [0.01]
                         :n 100
                         :mode [:xex]
                         :AO true
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0)))]
                         :maximize? false
                         :temperature [(int-array (concat (map #(* 3 (inc %)) (range 5)) (repeat 295 15)))]
                         :dampening nil
                         :momentum-lr [0.03]
                         :max-n 300
                        ;;  :dist-acc 0.01
                        ;;  :kde true 
                        ;;  :bandwidths [0.25 0.1]
                         }
     :reward-transformation :log-diff
    ;;  :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.01} {:e 0.03} {:e 0.1} {:e 0.3} {:e 1} {:e 3}]
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))

