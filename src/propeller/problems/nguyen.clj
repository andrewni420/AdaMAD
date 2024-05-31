;; pagie_hogeweg.clj
;; 
;; Pagie and Hogeweg 1997: Ludo Pagie, Paulien Hogeweg: Evolutionary Consequences of Coevolving Targets. 
;; Evolutionary Computation 5(4): 401-418 (1997). 
;;
;; Kyle Harrington, kyleh@cs.brandeis.edu, 2011

(ns propeller.problems.nguyen
  (:require [clojure.math.numeric-tower :as math]
            [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions
                                                 def-instruction
                                                 make-instruction]]
            [propeller.tools.math :as math-tools]
            [propeller.gp :as gp]))

(defn sol 
  [x]
  (+ (Math/log (inc x)) (Math/log (inc (* x x)))))

(def testing-data (map #(vector % (sol %)) (repeatedly 20 #(* (rand) 2))))
(def training-data testing-data)

(def error-function (gp/default-error-function :input-fn (fn [input] {:in1 input})
                                               :error (fn [target output]
                                                        (if (number? output)
                                                          (math-tools/abs (- output target))
                                                          1000))
                                               :out-stacks :float))

(def-instruction
  :float_pd
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state #(if (zero? %2) 0. (/ %1 %2)) [:float :float] :float)))


(def instructions
  (concat (list
           (fn [] (- (rand 2) 1))
            ;;; end ERCs
           :in1
           'close
            ;;; end input instructions
           :float_subtract
           :float_mult
           :float_add
           :float_pd)))

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
