(ns propeller.problems.PSB1.small-or-large
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


(def train-and-test-data (psb2/fetch-examples "data" "small-or-large" 200 2000))
(def train-data (:train train-and-test-data))
(def test-data (:test train-and-test-data))

; Random integer between -100 and 100
(defn random-int [] (- (rand-int 201) 100))

(def instructions
  (utils/not-lazy
    (concat
      ;;; stack-specific instructions
      (get-stack-instructions #{:exec :integer :boolean :string :print})
      ;;; input instructions
      (list :in1)
      ;;; close
      (list 'close)
      ;;; ERCs (constants)
      (list "" "small" "large" random-int))))

(defn error-function
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
  " Runs the top-level genetic programming function, giving it a map of
arguments with defaults that can be overridden from the command line
or through a passed map. "
  [& args]
  (maxbandit/gp
   (merge
    {:instructions             instructions
     :error-function           error-function
     :training-data            train-data
     :testing-data             test-data
     :max-generations          5000
     :population-size          5
     :max-initial-plushy-size  250
     :step-limit               2000
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

