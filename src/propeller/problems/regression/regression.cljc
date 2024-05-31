;; This file implements a version of the Nguyen-F1 symbolic regression problem, as described in various
;; publications in the genetic programming literature including:
;;
;;   Makke, N., Chawla, S. Interpretable scientific discovery with symbolic regression: a review. 
;;   Artif Intell Rev 57, 2 (2024). https://doi.org/10.1007/s10462-023-10622-0
;;
;; Note however that it may differ in some respects from the problem used elsewhere, for example
;; in the data ranges and gentic programming function sets which are not always fully documented
;; in the literature. For this reason, while this code can be used as an example and for comparing
;; different configurations of the present system, results obtained with this code may not be directly
;; comparable to those published in the literature.

(ns propeller.problems.regression.regression
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.tools.math :as math]
            [propeller.tools.maxbandit :as maxbandit]
            [propeller.gp :as gp]
            [propeller.tools.gesmr :as gesmr]
            [clojure.pprint :as pprint]
            #?(:cljs [cljs.reader :refer [read-string]])))

(def instructions
  (list :float_add
        :float_subtract
        :float_mult
        :float_div
        :float_sin
        :float_cos
        :float_log
        1.0))

(defn unif
  "Uniformly samples n-samples points in n-dim dimensional space between low and high"
  [n-samples n-dim low high]
  (into [] (repeatedly n-samples (fn [] (into [] (repeatedly n-dim (fn [] (+ low (* (rand) (- high low))))))))))

(def target-functions-builder
  {:koza1 {:num-inputs 1 
           :function (fn [x] (+ (math/pow x 4) (math/pow x 3) (math/pow x 2) (math/pow x 1)))
           :train (repeatedly 20 (fn [] [(dec (* 2 (rand)))]))}
   :koza2 {:num-inputs 1 
           :function (fn [x] (+ (math/pow x 5) (- (* 2 (math/pow x 3))) x))
           :train (repeatedly 20 (fn [] [(dec (* 2 (rand)))]))}
   :koza3 {:num-inputs 1 
           :function (fn [x] (+ (math/pow x 6) (- (* 2 (math/pow x 4))) (math/pow x 2)))
           :train (repeatedly 20 (fn [] [(dec (* 2 (rand)))]))}
   :nguyen1 {:num-inputs 1
             :function (fn [x] (+ (math/pow x 3) (math/pow x 2) x))
             :train (repeatedly 20 (fn [] [(dec (* 2 (rand)))]))}
   :nguyen2 {:num-inputs 1 
             :function (fn [x] (+ (math/pow x 4) (math/pow x 3) (math/pow x 2) x))
             :train (repeatedly 20 (fn [] [(dec (* 2 (rand)))]))}
   :nguyen3 {:num-inputs 1 
             :function (fn [x] (+ (math/pow x 5) (math/pow x 4) (math/pow x 3) (math/pow x 2) x))
             :train (repeatedly 20 (fn [] [(dec (* 2 (rand)))]))}
   :nguyen4 {:num-inputs 1
             :function (fn [x] (+ (math/pow x 6) (math/pow x 5) (math/pow x 4) (math/pow x 3) (math/pow x 2) x))
             :train  (repeatedly 20 (fn [] [(dec (* 2 (rand)))]))}
   :nguyen5 {:num-inputs 1 
             :function (fn [x] (dec (* (math/sin (math/pow x 2)) (math/cos x))))
             :train (repeatedly 20 (fn [] [(dec (* 2 (rand)))]))}
   :nguyen6 {:num-inputs 1
             :function (fn [x] (+ (math/sin x) (math/sin (+ x (* x x)))))
             :train (repeatedly 20 (fn [] [(dec (* 2 (rand)))]))}
   :nguyen7 {:num-inputs 1
             :function (fn [x] (+ (math/log (inc x)) (math/log (inc (* x x)))))
             :train (repeatedly 20 (fn [] [(* 2 (rand))]))}
   :nguyen8 {:num-inputs 1 
             :function math/sqrt
             :train (repeatedly 20 (fn [] [(* 4 (rand))]))}
   :nguyen9 {:num-inputs 2 
             :function (fn [x y] (+ (math/sin x) (math/sin (* y y))))
             :train (repeatedly 100 (fn [] (into [] (repeatedly 2 #(dec (* 2 (rand)))))))}
   :nguyen10 {:num-inputs 2 
              :function (fn [x y] (* 2 (math/sin x) (math/cos y)))
              :train (repeatedly 100 (fn [] (into [] (repeatedly 2 #(dec (* 2 (rand)))))))}
   :nguyen11 {:num-inputs 2 
              :function math/pow
              :train (repeatedly 100 (fn [] (into [] (repeatedly 2 #(* 2 (rand))))))}
   :nguyen12 {:num-inputs 2 
              :function (fn [x y] (+ (math/pow x 4) (- (math/pow x 3)) (* 0.5 y y) (- y)))
              :train (repeatedly 100 (fn [] (into [] (repeatedly 2 #(dec (* 2 (rand)))))))}
   :vladislavleva1 {:num-inputs 2
                    :function (fn [x1 x2] (/ (math/exp (- (math/square (dec x1)))) (+ 1.2 (math/square (- x2 2.5)))))
                    :additional-instructions (map float (range 2 11))}
   :vladislavleva2 {:num-inputs 1
                    :function (fn [x] (* (math/exp (- x)) x x x (math/cos x) (math/sin x) (dec (* (math/cos x) (math/sin x) (math/sin x)))))
                    :additional-instructions (map float (range 2 11))}
   :vladislavleva3 {:num-inputs 2
                    :function (fn [x1 x2] (* (- x2 5) (math/exp (- x1)) x1 x1 x1 (math/cos x1) (math/sin x1) (dec (* (math/cos x1) (math/sin x1) (math/sin x1)))))
                    :additional-instructions (map float (range 2 11))}
   :vladislavleva4 {:num-inputs 5
                    :function (fn [x1 x2 x3 x4 x5] (/ 10 (+ 5 (math/square (- x1 3)) (math/square (- x2 3)) (math/square (- x3 3)) (math/square (- x4 3)) (math/square (- x5 3)))))
                    :additional-instructions (map float (range 2 11))}
   :vladislavleva5 {:num-inputs 3
                    :function (fn [x1 x2 x3] (/ (* 30 (dec x1) (dec x3)) (* x2 x2 (- x1 10))))
                    :additional-instructions (map float (range 2 11))}
   :vladislavleva6 {:num-inputs 2
                    :function (fn [x1 x2] (* 6 (math/sin x1) (math/cos x2)))
                    :additional-instructions (map float (range 2 11))}
   :vladislavleva7 {:num-inputs 2
                    :function (fn [x1 x2] (+ (* (- x1 3) (- x2 3)) (* 2 (math/sin (* (- x1 4) (- x2 4))))))
                    :additional-instructions (map float (range 2 11))}
   :vladislavleva8 {:num-inputs 2
                    :function (fn [x1 x2] (/ (+ (math/pow (- x1 3) 4) (math/pow (- x2 3) 3) (- 3 x2)) (+ 10 (math/pow (- x2 2) 4))))
                    :additional-instructions (map float (range 2 11))}
   :r1 {:num-inputs 1
        :function (fn [x] (/ (math/pow (inc x) 3) (+ (* x x) (- x) 1)))
        :additional-instructions (map float (range 2 11))
        :train (map vector (range -1 1.01 0.1))}
   :r2 {:num-inputs 1
        :function (fn [x] (/ (- (inc (math/pow x 5)) (* 3 x x x)) (inc (* x x))))
        :additional-instructions (map float (range 2 11))
        :train (map vector (range -1 1.01 0.1))}
   :r3 {:num-inputs 1
        :function (fn [x] (/ (+ (math/pow x 5) (math/pow x 6)) (+ (math/pow x 4) (math/pow x 3) (math/pow x 2) x 1)))
        :additional-instructions (map float (range 2 11))
        :train (map vector (range -1 1.01 0.1))}
   :livermore1 {:num-inputs 1
                :function (fn [x] (+ (/ 1 3) x (math/sin (* x x))))
                :train (unif 100 1 -10 10)
                :additional-instructions (concat [:float_exp] (map float (range 2 11)))}
   :livermore2 {:num-inputs 1
                :function (fn [x] (- (* (math/sin (* x x)) (math/cos x)) 2))
                :train (unif 20 1 -1 1)
                :additional-instructions (concat [:float_exp] (map float (range 2 11)))}
   :livermore3 {:num-inputs 1
                :function (fn [x] (dec (* (math/sin (* x x x)) (math/cos (* x x)))))
                :train (unif 20 1 -1 1)
                :additional-instructions (concat [:float_exp] (map float (range 2 11)))}
   :livermore4 {:num-inputs 1
                :function (fn [x] (+ (math/log (inc x)) (math/log (inc (* x x))) (math/log x)))
                :train (unif 20 1 0 2)
                :additional-instructions (concat [:float_exp] (map float (range 2 11)))}
   :livermore5 {:num-inputs 2
                :function (fn [x y] (+ (math/pow x 4) (- (* x x x)) (* x x) y))
                :train (unif 20 2 0 1)
                :additional-instructions (concat [:float_exp] (map float (range 2 11)))}
   :livermore6 {:num-inputs 1
                :function (fn [x] (+ (* 4 x x x x) (* 3 x x x) (* 2 x x) x))
                :train (range -4.0 4.01 0.1)
                :test (range -4.0 4.001 0.05)
                ;; :train (unif 20 1 -1 1)
                ;; :additional-instructions (concat [:float_exp] (map float (range 2 11)))
                }
   :livermore7 {:num-inputs 1
                :function (fn [x] (/ (- (math/exp x) (math/exp (- x))) 2))
                :train (unif 20 1 -1 1)
                :additional-instructions (concat [:float_exp] (map float (range 2 11)))}
   :livermore8 {:num-inputs 1
                :function (fn [x] (/ (+ (math/exp x) (math/exp (- x))) 2))
                :train (unif 20 1 -1 1)
                :additional-instructions (concat [:float_exp] (map float (range 2 11)))}
   :livermore9 {:num-inputs 1
                :function (fn [x] (transduce (map #(math/pow x %)) + (range 1 10)))
                :train (unif 20 1 -1 1)
                :additional-instructions (concat [:float_exp] (map float (range 2 11)))}
   :livermore10 {:num-inputs 2
                :function (fn [x y] (* 6 (math/sin x) (math/cos y)))
                :train (unif 20 2 0 1)
                :additional-instructions (concat [:float_exp] (map float (range 2 11)))}
   :korns1-int {:num-inputs 5 
                :function (fn [x0 x1 x2 x3 x4] (+ 2 (* 24 x3)))
                :train (unif 100 5 -50 50)}})

;; nguyen10: 0.1 = 49
;; nguyen11: 0.1 = 0
;; nguyen12: 0.1 = 0
;; nguyen2: 0.05 = 15, 0.1 = 17, 0.2 = 17, adaptive = 9
;; nguyen3: 0.1 = 2
;; nguyen4-range: 0.05 = 20, 0.1 = 23, 0.2 = 18, adaptive = 22
;; nguyen5: 0.01 = 0, 0.05 = 7, 0.1 = 24, 0.2 = 38, 0.5 = 29, adaptive = 39
;; nguyen6: 0.1 = 40 
;; nguyen7: 0.1 = 2
;; nguyen8: 0.1 = 0
;; nguyen9: 0.1 = 50

;; livermore1: 0.1 = 1
;; livermore2: 0.1 = 4, adaptive = 1
;; livermore3: 0.1 = 0
;; livermore4: 0.1 = 0
;; livermore5: 0.1 = 0
;; livermore6: 0.1 = 0
;; livermore7: 0.1 = 7
;; livermore8: 0.1 = 3
;; livermore9: 0.1 = 0
;; livermore10: 0.1 = 7, adaptive = 13, 




(def range-targets
  (concat (map #(let [kw (keyword (str "nguyen" %))
                      target (get target-functions-builder kw)]
                  [(keyword (str "nguyen" % "-range"))
                   (assoc target :train (map vector (range -4.0 4.0 0.1)) :test (map vector (range -4.0 4.0 0.05)))]) (range 1 7))
          (map #(let [kw (keyword (str "nguyen" %))
                      target (get target-functions-builder kw)]
                  [(keyword (str "nguyen" % "-range"))
                   (assoc target :train (map vector (range 0. 8.0 0.1)) :test (map vector (range 0.0 8.0 0.05)))]) (range 7 9))))

(def target-functions 
  (merge target-functions-builder (into {} range-targets)))

(defn make-default-target 
  [target]
  (merge {:num-inputs 1 
          :test (:train target)
          :threshold 0.01} 
         target))



(def error-function
  (gp/default-error-function :out-stacks :float
                             :error (fn [correct-output output]
                                      (if (= output :no-stack-item)
                                        10000000
                                        (math/abs (- correct-output output))))))

#_(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (gp/gp
   (merge
    {:instructions              instructions
     :error-function            error-function
     :training-data             (:train train-and-test-data)
     :testing-data              (:test train-and-test-data)
     :downsample?               false
     :solution-error-threshold  0.1
     :max-generations           300
     :population-size           1000
     :max-initial-plushy-size   50
     :step-limit                100
     :parent-selection          :epsilon-lexicase
     :umad-rate                 0.05
     :variation                 {:umad 1.0}
     :simplification?           true}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))

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
       :solution-error-threshold  0.1
       :max-generations         300
       :population-size         1000
       :max-initial-plushy-size 50
       :step-limit              100
       :restart 0.0
       :parent-selection        :epsilon-lexicase
       :tournament-size         5
       :umad-rate               0.05
       :variation               {:umad 1 :crossover 0}
       :elitism                 false}
      (apply hash-map (map #(if (string? %) (read-string %) %) args)))))



#_(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (maxbandit/gp
   (merge
    {;;:mapper mapv
    ;;  :downsample?                 true ; wether to use downsampling
    ;;  :ds-function                 :case-maxmin ; :case-rand, case-maxmin, case-maxmin-auto
    ;;  :downsample-rate             0.05 ; proportion of data used in downsample
    ;;  :ds-parent-rate              0.01 ; proportion of parents used to evaluate case distances
    ;;  :ds-parent-gens              10 ; generations between computation of parent distances
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
    ;;  :transformations [maxbandit/tile-to-parameters-umad maxbandit/parameters-to-tile-umad]
     :transformations [#(maxbandit/tile-to-parameters-random % :basis basis :low [-5 -5 -5] :high [4.9999 4.9999 4.9999] :sigma [0.5 0.5 0.5]) maxbandit/parameters-to-tile-random]
     :bandit-parameters {:num-bandits 1
                         :l [-5 -5 -5]
                         :r [5 5 5]
                         :to (into [] (repeat 3 (into [] (map (partial * 0.5) (range 5))))) #_[[0 0.5 1 1.5 2] [0 0.5 1 1.5 2] [0 0.5 1 1.5 2] [0 0.5 1 1.5 2]]
                         :ta (into [] (repeat 3 (into [] (map (partial * 0.5) (range 5 11))))) #_[[2.5 3 3.5 4 4.5 5] [2.5 3 3.5 4 4.5 5] [2.5 3 3.5 4 4.5 5] [2.5 3 3.5 4 4.5 5]]
                         :num-codings 20
                         :d [1]
                         :acc (into [] (repeat 3 0.5)) #_[0.5 0.5 0.5 0.5]
                         :lr [{:method :momentum
                               :momentum-lr 0.1
                               :lr 0.001
                               :dampening 1}]
                         :s [50]
                         :delta [0.01]
                         :n 300
                         :mode [:argmax]
                         :AO true
                         :xi* [0.01]
                         :epsilon [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))]
                         :max-n 100
                         :sigma 0}
     :reward-transformation :log-diff
    ;;  :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.01} {:e 0.03} {:e 0.1} {:e 0.3} {:e 1} {:e 3}]
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))



#_(defn run
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [argmap]
  (maxbandit2/gp
   (merge
    {;;:mapper mapv
     :instructions            instructions
     :error-function          error-function
     :max-generations         300
     :population-size         [1000 1000]
     :max-initial-plushy-size 50
     :step-limit              100
     :parent-selection        :epsilon-lexicase
     :tournament-size         5
     :update-freq 100
     :report-interval 1
     :bandit-parameters {:num-bandits 1
                         :l [-10 -5 -5]
                         :r [10 5 5]
                         :to (into [] (repeat 3 (into [] (map (partial * 0.5) (range 5))))) #_[[0 0.5 1 1.5 2] [0 0.5 1 1.5 2] [0 0.5 1 1.5 2] [0 0.5 1 1.5 2]]
                         :ta (into [] (repeat 3 (into [] (map (partial * 0.5) (range 5 11))))) #_[[2.5 3 3.5 4 4.5 5] [2.5 3 3.5 4 4.5 5] [2.5 3 3.5 4 4.5 5] [2.5 3 3.5 4 4.5 5]]
                         :num-codings 20
                         :d [1]
                         :acc (into [] (repeat 3 0.5)) #_[0.5 0.5 0.5 0.5]
                         :lr (into [] (map (fn [lr] {:method :momentum :lr (math/pow 10 (- (/ lr 9) 4)) :dampening 1 :momentum-lr 0.1})) (range 10))
                         #_[{:method :momentum
                               :momentum-lr 0.1
                               :lr 0.001
                               :dampening 1}]
                         :s [50]
                         :delta [0.01]
                         :n 300
                         :mode [:argmax]
                         :AO true
                         :xi* [0.01]
                         :epsilon [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))]
                         :max-n 100
                         :sigma 2}
     :generations-per-update 1
     :reward-transformation :log-diff-0.01
        ;;  :parameter-override {:e 0.1}
        ;;  :parameter-analysis [{:e 0.0001} {:e 0.0003} {:e 0.001} {:e 0.003} {:e 0.01} {:e 0.03} {:e 0.1}]
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    argmap)))

(defn run
  [argmap]
  (maxbandit/gp
   (merge
    {;;:mapper mapv 
     :n-rates 100 
     :rate-sigma 2
     :rate-eta 10
     :instructions            instructions
     :error-function          error-function
     :max-generations         300
     :population-size         [1000 1000]
     :max-initial-plushy-size 50
     :step-limit              100
     :parent-selection        :epsilon-lexicase
     :tournament-size         5
     :update-freq 100
     :report-interval 1
     :transformations [maxbandit/tile-to-parameters-umad maxbandit/parameters-to-tile-umad]
     :bandit-parameters {:num-bandits 5
                         :l [-10]
                         :r [10]
                         :to [[0 0.03 0.06 0.09 0.12 0.15]] #_[[0 0.03 0.06 0.09 0.12 0.15 0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39 0.42 0.45 0.48]]
                         :ta [[0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39]] #_[[0.51 0.54 0.57 0.6 0.63 0.66 0.69 0.72 0.75 0.78 0.81 0.84 0.87 0.9]]
                         :num-codings 20
                         :d [1]
                         :acc [0.03]
                         :lr (into [] (map (fn [lr] {:method :momentum :lr (math/pow 10 (- (/ lr 9) 4)) :dampening 1 :momentum-lr 0.1})) (range 10))
                         #_[{:method :momentum
                               :momentum-lr 0.1
                               :lr 0.001
                               :dampening 1
                            ;;    :alpha 0.0003
                            ;;    :beta1 0.03
                            ;;    :beta2 0.001
                              ;;  :warm-start 5
                               }]
                         :s [50]
                         :delta [0.01]
                         :n 100
                         :mode [:argmax]
                         :AO true
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0.01)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                        ;;  :dampening 1
                        ;;  :momentum-lr [0.03]
                         :max-n 100
                        ;;  :legacy true
                         :sigma 3}
     :generations-per-update 1
     :reward-transformation :log-diff-0.01
    ;;  :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.0001} {:e 0.0003} {:e 0.001} {:e 0.003} {:e 0.01} {:e 0.03} {:e 0.1}]
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    argmap)))


(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (let [{problem :problem
         :as argmap} (apply hash-map (map #(if (string? %) (read-string %) %) args))
        problem (if (symbol? problem) (keyword problem) problem)
        argmap (assoc argmap :problem problem)
        {train :train
         test :test
         num-inputs :num-inputs
         threshold :threshold
         function :function
         additional-instructions :additional-instructions
         :as target} (make-default-target (get target-functions problem))
        train (mapv (fn [x] [x (apply function (if (number? x) [x] x))]) train)
        test (mapv (fn [x] [x (apply function (if (number? x) [x] x))]) test)
        instructions (concat instructions
                             (map #(keyword (str "in" (inc %)))
                                  (range num-inputs))
                             additional-instructions)
        ;;  basis (maxbandit/make-random-basis 2 (count instructions))
        ;; transformations [#(maxbandit/tile-to-parameters-random-umad % :basis basis :low [-10 -4.999 -4.999] :high [9.999 4.999 4.999] :sigma [0.5 0.5])
        ;;                  maxbandit/parameters-to-tile-random]
        ]
    ;; (pprint/pprint {:basis basis
    ;;                 :instructions (mapv str instructions)})
    (run (merge {:solution-error-threshold threshold
                ;;  :transformations transformations
                 :training-data train
                 :testing-data test
                 :instructions instructions}
                argmap))))
