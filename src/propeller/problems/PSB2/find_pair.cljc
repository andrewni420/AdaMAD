(ns propeller.problems.PSB2.find-pair
  (:require [psb2.core :as psb2]
            [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.utils :as utils]
            [propeller.push.instructions :refer [def-instruction get-stack-instructions]]
            [propeller.push.state :as state]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])))

(def train-and-test-data (psb2/fetch-examples "data" "find-pair" 200 2000))
(def train-data (:train train-and-test-data))
(def test-data (:test train-and-test-data))

(defn random-int [] (- (rand-int 201) 100))

(defn map-vals-input
  "Returns all the input values of a map"
  [i]
  (vals (select-keys i [:input1 :input2])))

(defn map-vals-output
  "Returns the output values of a map"
  [i]
  (vals (select-keys i [:output1 :output2])))

(def-instruction :output-one
  ^{:stacks #{:integer :output}}
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (state/peek-stack state :integer)]
        (assoc-in state [:output :out1] top-int)))))

(def-instruction :output-two
  ^{:stacks #{:integer :output}}
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (state/peek-stack state :integer)]
        (assoc-in state [:output :out2] top-int)))))

(def instructions
  (utils/not-lazy
   (concat
      ;;; stack-specific instructions
    (get-stack-instructions #{:exec :integer :vector_integer :boolean})
    (list :output-one :output-two)
      ;;; input instructions
    (list :in1 :in2)
      ;;; close
    (list 'close)
      ;;; ERCs (constants)
    (list -1 0 1 2 random-int))))


(defn error-function
  [argmap data individual]
  (let [program (genome/plushy->push (:plushy individual) argmap)
        inputs (map (fn [i] (map-vals-input i)) data)
        correct-outputs (map (fn [i] (map-vals-output i)) data)
        outputs (map (fn [input]
                       (:output
                        (interpreter/interpret-program
                         program
                         (assoc state/empty-state :input {:in1 (nth input 0)
                                                          :in2 (nth input 1)})
                         (:step-limit argmap))))
                     inputs)
        outputs-1 (map #(:out1 %) outputs)
        outputs-2 (map #(:out2 %) outputs)
        ;_ (prn {:o1 outputs-1 :o2 outputs-2})
        errors (map (fn [correct-output output-1 output-2]
                      (if (not (and (number? output-2) (number? output-1)))
                        100000
                        (+  (math/abs (- (first correct-output) output-1))
                            (math/abs (- (second correct-output) output-2)))))
                    correct-outputs outputs-1 outputs-2)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error #?(:clj  (apply +' errors)
                           :cljs (apply + errors)))))

#_(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (gp/gp
   (merge
    {:instructions            instructions
     :error-function          error-function
     :training-data           train-data
     :testing-data            test-data
     :case-t-size             (count train-data)
     :ds-parent-rate          0
     :ds-parent-gens          1
     :max-generations         300
     :population-size         1000
     :max-initial-plushy-size 250
     :step-limit              2000
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.1
     :variation               {:umad 1.0 :crossover 0.0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args))))
  (#?(:clj shutdown-agents)))


(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (maxbandit/downsample-gp
   (merge
    {;;:mapper concurrent/service-mapper
     :downsample?                 true ; wether to use downsampling
     :ds-function                 :case-maxmin ; :case-rand, case-maxmin, case-maxmin-auto
     :downsample-rate             0.05 ; proportion of data used in downsample
     :ds-parent-rate              0.01 ; proportion of parents used to evaluate case distances
     :ds-parent-gens              10 ; generations between computation of parent distances
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
     :update-freq 1000
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
                         :mode [:argmax]
                         :AO false
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5.) (range 5)) (repeat 5995 0)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 5995 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                                                ;;  :dampening 1
                                                ;;  :momentum-lr [0.03]
                         :max-n 100
                         :sigma 3
                                                ;;  :legacy true
                         }
     :generations-per-update 1
     :reward-transformation :log-diff
    ;;  :parameter-override {:e 0.05}
    ;;  :parameter-analysis [{:e 0.0001} {:e 0.0003} {:e 0.001} {:e 0.003} {:e 0.01} {:e 0.03} {:e 0.1}]
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
