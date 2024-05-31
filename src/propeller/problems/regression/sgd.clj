(ns propeller.problems.regression.sgd
  (:require [propeller.tools.distributions :as dist]
            [propeller.tools.math :as math]
            [clojure.pprint :as pprint]))

(defn new-ind 
  [std n depth & {:keys [id] :or {id 0}}]
  {:id id 
   :program (into [] (cons (into [] (dist/rand-norm {:n n :mu 0 :sigma std}))
                           (repeatedly (dec depth) #(into [] (repeat n 0))))) })

(defn mutate
  [ind strength]
  (let [{program :program} ind
        new-program (into []
                          (rest 
                           (reductions (fn [acc mut]
                                        (let [acc (if (number? acc) (dist/rand-norm {:n (count mut) :mu 0 :sigma acc}) acc)]
                                          (mapv + acc mut)))
                                      strength
                                      program)))]
    (assoc ind :program
           new-program)))

(defn evaluate 
  [ind evaluator]
  (assoc ind :error (evaluator (peek (:program ind)))))

(defn linear 
  [point]
  (transduce (map math/abs) + point))

(defn square 
  [point]
  (transduce (map math/square) + point))

(defn truncation-selection 
  [pop argmap]
  (let [{size :truncation-size
         elitism :elitism} argmap
        selected (into [] (take size (sort-by :error pop)))
        pop (into [] (repeatedly ((if elitism dec identity) (count pop)) #(rand-nth selected)))]
    (if elitism (conj pop (first selected)) pop)))

(defn best-individual 
  [pop]
  (apply min-key :error pop))


(defn initialize-generation 
  [{:keys [popsize init-std depth dim] :or {popsize 10 init-std 1 dim 1 depth 1}}]
  (mapv #(new-ind init-std dim depth :id %) (range popsize)))

(defn mutate-generation
  [pop gen argmap]
  ((if (:elitism argmap)
     #(conj % (best-individual pop))
     identity)
   (into []
         (map-indexed #(assoc (mutate %2 (:std argmap))
                              :id (+ (* gen (count pop)) %1))
                      ((if (:elitism argmap)
                         rest
                         identity)
                       pop)))))

(defn evaluate-generation
  [pop argmap]
  (mapv #(evaluate % (:error-function argmap)) pop))

(defn select-generation
  [pop argmap]
  (condp = (:selector argmap)
    :single (into [] (repeat (count pop) (best-individual pop)))))

(defn step-generation
  [pop gen argmap]
  (if pop 
    (-> pop 
        (select-generation argmap)
        (mutate-generation gen argmap)
        (evaluate-generation argmap))
    (-> (initialize-generation argmap)
        (evaluate-generation argmap))))

(defn report 
  [pop gen argmap]
  (println)
  ;; (pprint/pprint {:generation gen})
  ;; (pprint/pprint pop)
  (pprint/pprint {:generation gen 
                  :best-fitness (best-individual pop)}))


(defn ga 
  [argmap]
  (loop [pop nil 
         gen 0]
    (when (and pop (:verbose argmap)) (report pop gen argmap))
    (if (= gen (:generations argmap))
      pop 
      (recur (step-generation pop gen argmap)
             (inc gen)))))

(defn get-ci 
  [n coll]
  (let [mean (math/mean coll)
        std (math/std coll)]
    {:mean mean
     :std std
     :ci [(- mean (/ (* std 2.96) n))
          (+ mean (/ (* std 2.96) n))]}))

(defn average-final-fitness 
  [iterations argmap]
  (let [bests (into [] (repeatedly iterations #(:error (best-individual (ga argmap)))))
        log-bests (mapv math/log bests)]
    (pprint/pprint {:best-error (get-ci iterations bests)
                    :best-log-error (get-ci iterations log-bests)})
    (math/mean bests)))


(defn -main
  []
  (average-final-fitness 1000
                         {:generations 1
                          :depth 1
                          :std 0.1
                          :init-std 1
                          :dim 10
                          :popsize 1
                          :elitism true
                          :verbose false
                          :error-function linear
                          :selector :single}))






