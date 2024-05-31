;; pagie_hogeweg.clj
;; 
;; Pagie and Hogeweg 1997: Ludo Pagie, Paulien Hogeweg: Evolutionary Consequences of Coevolving Targets. 
;; Evolutionary Computation 5(4): 401-418 (1997). 
;;
;; Kyle Harrington, kyleh@cs.brandeis.edu, 2011

(ns propeller.problems.pagie
  (:require [clojure.math.numeric-tower :as math]
            [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions
                                                 def-instruction
                                                 make-instruction]]
            [propeller.tools.math :as math-tools]
            [propeller.gp :as gp]))

(defn data-point-2D
  "Generate a 2D data point from:
   f(x,y) = 1 / (1 + x^-4) + 1 / ( 1 + y^-4 )"
  [x y]
  (+ (/ (+ 1 (math/expt x -4)))
     (/ (+ 1 (math/expt y -4)))))

(def data (doall (for [x (range -5.0 5.01 0.4) ; Range is exclusive on the end
                       y (range -5.0 5.01 0.4)]
                   [x y (data-point-2D x y)])))

(def testing-data (map #(vector (take 2 %) (nth % 2)) data))
(def training-data (take (int (* 0.7 (count data)))
                         (shuffle testing-data)))

(def error-function (gp/default-error-function :error (fn [target output]
                                                        (if (number? output)
                                                          (math-tools/abs (- output target))
                                                          1000))
                                               :out-stacks :float))

#_(defn hit-error-function
  "Error function based on Koza's hit criteria."
  [num-samples program]
  (doall
    (for [row (if (== (count data) num-samples)
                data
                (take num-samples (shuffle data)))]
      (let [state (run-push program 
                            (assoc (make-push-state)
                                   :auxiliary
                                   (butlast row)))
            top-float (top-item :float state)]
        (if (number? top-float)
          (if (< (math/abs (- top-float (last row))) 0.01) 0 1)
          1)))))

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
           :in2
           'close
            ;;; end input instructions
           :float_subtract
           :float_mult
           :float_add
           :float_pd)))

#_(defn problem-specific-report [best population generation sampled-error-function report-simplifications] 
  (let [errors (:errors (error-function (count data) best))
        hit-errors (hit-error-function (count data) (:program best))
        total-error (apply + errors)
        hit-total-error (apply + hit-errors)]
    #_(println "Best's errors on full data set:" errors)
    (println "--- Pagie-Hogeweg Problem Specific Report ---")
    (println "Best's total error on full data set:" total-error)
    (println "Best's total error (hits) on full data set:" hit-total-error)
    (if (zero? hit-total-error)
      (assoc best :success true)
      best)))


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
