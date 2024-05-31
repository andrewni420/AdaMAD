;;Kejizer: +, x, -n, 1/n, sqrt, N(0,5)
;;Koza: +, -, x, /, sin, cos, exp, ln(|n|)
;;Korns: +, -, x, /, sin, cos, exp, ln(|n|), n^2, n^3, sqrt, tan, tanh
;;Vladislavleva A: +, -, x, /, n^2, n^eps, n+eps, n*eps (eps = erc[-5,5])
;;Vladislavleva B: +, -, x, /, n^2, e^n, e^-n, n^eps, n+eps, n*eps (eps = erc[-5,5])
;;Vladislavleva A: +, -, x, /, n^2, e^n, e^-n, sin, cos, n^eps, n+eps, n*eps (eps = erc[-5,5])

(ns propeller.problems.keijzer
  (:require [clojure.math.numeric-tower :as math]
            [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions
                                                 def-instruction
                                                 make-instruction]]
            [propeller.tools.math :as math-tools]
            [propeller.gp :as gp]
            [propeller.tools.distributions :as dist]))

(defn sol 
  [i]
  (transduce (map (partial / 1)) + (range 1 (inc i))))

(def testing-data (map #(vector (float %) (sol %)) (range 1 121)))
(def training-data (take 50 testing-data))

(def error-function (gp/default-error-function :input-fn (fn [input] {:in1 input})
                                               :error (fn [target output]
                                                        (if (number? output)
                                                          (math-tools/abs (- output target))
                                                          1000))
                                               :out-stacks :float))

(def-instruction
  :sqrt
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state #(if (> 0 %) (- (Math/sqrt (- %))) (Math/sqrt %)) [ :float] :float)))

(def-instruction
  :float_inv
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state #(if (zero? %) % (/ 1 %)) [:float] :float)))

(def-instruction
  :float_neg
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state - [:float] :float)))


(def instructions
  (concat (list
           (fn [] (first (dist/get-distribution :normal 1 5 0)))
            ;;; end ERCs
           :in1
           'close
            ;;; end input instructions
           :float_neg 
           :float_inv
           :float_mult
           :float_add
           :sqrt)))

(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (gp/gp
   (merge
    {:instructions            instructions
     :error-function          error-function
     :training-data           training-data
     :testing-data            testing-data
     :max-generations         300
     :population-size         1000
     :max-initial-plushy-size 30
     :step-limit              50
     :restart 0.0
     :solution-error-threshold 0.01
     :parent-selection        :epsilon-lexicase
     :tournament-size         5
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
