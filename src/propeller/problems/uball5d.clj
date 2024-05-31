;; uball5d.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2017

(ns propeller.problems.uball5d
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions
                                                 def-instruction
                                                 make-instruction]]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]))

;;;;;;;;;;;;
;; Floating point symbolic regression of the Vladislavleva 2009 UBall5D problem.

;; Each training case will be in the form [[x1 x2 x3 x4 x5] y], where the xs are
;; inputs and y is the output.

(def uball5d-training-cases
  (for [xseq (repeatedly 1024
                         (fn [] (repeatedly 5
                                            #(+ 0.05 (rand 6)))))]
    [xseq
     (->> (map (fn [x] (#(* % %) (- x 3)))
               xseq)
          (reduce +)
          (+ 5)
          (/ 10.0))]))

(def uball5d-testing-cases
  (for [xseq (repeatedly 5000
                         (fn [] (repeatedly 5
                                            #(- (rand 6.6) 0.25))))]
    [xseq
     (->> (map (fn [x] (#(* % %) (- x 3)))
               xseq)
          (reduce +)
          (+ 5)
          (/ 10.0))]))


(def error-function (gp/default-error-function :error (fn [target output]
                                                        (if (number? output)
                                                          (Math/abs (- output target))
                                                          1000000))
                                               :out-stacks :float))

(def-instruction
  :float_pd
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state #(if (zero? %2) 0. (/ %1 %2)) [:float :float] :float)))

(def-instruction
  :float_exp
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state #(Math/exp %) [:float] :float)))

(def-instruction
  :float_log
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state #(Math/log %) [:float] :float)))

(def instructions
  (concat (list
           (fn [] (* 2 (- (rand) 0.5)))
            ;;; end ERCs
           :in1
           :in2
           :in3
           :in4 
           :in5
           'close
            ;;; end input instructions
           :float_subtract 
           :float_mult
           :float_add
           :float_cos
           :float_sin
           :float_pd
           :float_exp 
           :float_log
           )))


(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (gp/gp
   (merge
    {:instructions            instructions
     :error-function          error-function
     :training-data           uball5d-training-cases
     :testing-data            uball5d-testing-cases
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
