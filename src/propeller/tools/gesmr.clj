(ns propeller.tools.gesmr
  (:require [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.variation :as variation]
            [propeller.simplification :as simplification]
            [propeller.selection :as selection]
            [propeller.genome :as genome]
            [clojure.pprint :as pprint]
            [clojure.string :as string]
            [propeller.tools.concurrent :as concurrent]
            [propeller.push.limits :as l]
            [clojure.set :as set]
            [cheshire.core :as cheshire])
  (:import java.lang.Double
           java.lang.Class))

(set! *warn-on-reflection* true)

(defn make-mutation-rates 
  "Make mutation rates for the group elites paper"
  [n-rates]
  (mapv #(math/exp (- (* 10 (/ % n-rates)) 10)) (range n-rates)))


(defn make-default
  [argmap]
  (merge {:solution-error-threshold 0
          :restart 0
          :mapper concurrent/service-mapper 
          :n-rates 100 
          :rate-eta 0.1
          :update-freq 100
          :report-interval 1
          :rate-sigma 2
          :reward-transformation :log-diff
          :generations-per-update 1}
         (update argmap
                 :population-size
                 #(if (number? %) [% %] %))))

(defn finish-gp
  [generation best-ind success argmap]
  (let [{error-function :error-function} argmap]
    (gp/report-success generation best-ind (:error-function argmap) argmap)
    (if success
      (prn (merge {:success-generation generation
                   :total-test-error (:total-error (error-function argmap (:testing-data argmap) best-ind))}
                  (when (:simplification? argmap)
                    (let [simplified-plushy (simplification/auto-simplify-plushy (:plushy best-ind) error-function argmap)]
                      {:total-test-error-simplified (:total-error (error-function argmap (:testing-data argmap) (hash-map :plushy simplified-plushy)))}))))
      (do (prn {:success false
                :total-test-error ##Inf
                :generation generation
                :min-error (:total-error best-ind)})
          {:success? false
           :generation generation
           :min-error (:total-error best-ind)}))
    (shutdown-agents)))

(def reward-transformations-builder
  "Unique reward transformations for max bandits."
  {:selection-prob-diff (fn [ind parent dist]
                          (selection/assess-lexicase-difference (:errors ind) (:errors parent) dist))
   :selection-prob-abs (fn [ind _ dist]
                         (selection/assess-lexicase-difference (:errors ind) nil dist))
   :selection-prob-legacy (fn [ind parent dist]
                            (- (selection/assess-lexicase-prob-legacy (:errors ind) dist)
                               (selection/assess-lexicase-prob-legacy (:errors parent) dist)))
   :inverse-mean-abs (fn [ind _ _]
                       (math/mean (map #(/ 1 (+ 0.01 %)) (:errors ind))))
   :inverse-mean-diff (fn [ind parent _]
                        (- (math/mean (map #(/ 1 (+ 0.01 %)) (:errors ind)))
                           (math/mean (map #(/ 1 (+ 0.01 %)) (:errors parent)))))
   :inverse-mean-abs-scaled (fn [ind _ _]
                              (math/mean (map #(/ 1 (+ 1 %)) (:errors ind))))
   :inverse-mean-diff-scaled (fn [ind parent _]
                               (- (math/mean (map #(/ 1 (+ 1 %)) (:errors ind)))
                                  (math/mean (map #(/ 1 (+ 1 %)) (:errors parent)))))
   :inverse-rms-abs (fn [ind _ _]
                      (math/sqrt (math/mean (map #(/ 1 (math/square (+ 0.01 %))) (:errors ind)))))
   :inverse-rms-diff (fn [ind parent _]
                       (- (math/sqrt (math/mean (map #(/ 1 (math/square (+ 0.01 %))) (:errors ind))))
                          (math/sqrt (math/mean (map #(/ 1 (math/square (+ 0.01 %))) (:errors parent))))))
   :log-diff (fn [ind parent _]
               (- (math/mean (map #(math/log (inc %)) (:errors parent)))
                  (math/mean (map #(math/log (inc %)) (:errors ind)))))
   :log-diff-0.01 (fn [ind parent _]
                    (- (math/mean (map #(math/log (+ 0.01 %)) (:errors parent)))
                       (math/mean (map #(math/log (+ 0.01 %)) (:errors ind)))))
   :log-abs (fn [ind _ _]
              (- (math/mean (map #(math/log (+ 1 %)) (:errors ind)))))
   :selection-prob-plus-log (fn [ind parent dist]
                              (+ (selection/assess-lexicase-difference (:errors ind) (:errors parent) dist)
                                 (- (math/mean (map #(math/log (inc %)) (:errors parent)))
                                    (math/mean (map #(math/log (inc %)) (:errors ind))))))
   :selection-prob-times-log (fn [ind parent dist]
                               (selection/assess-lexicase-difference (:errors ind) (:errors parent) dist
                                                                     :ind-weight (- (math/mean (map #(math/log (inc %)) (:errors ind))))
                                                                     :parent-weight (- (math/mean (map #(math/log (inc %)) (:errors parent))))))})

(def reward-transformations
  "Reward transformations for max bandits. When not specifying -abs or -diff, defaults to -diff"
  (let [{sp :selection-prob-diff im :inverse-mean-diff} reward-transformations-builder]
    (assoc reward-transformations-builder
           :selection-prob sp
           :inverse-mean im)))



(defn step-generation
  [population rates argmap & {:keys [dist]}]
  (let [{mapper :mapper error-function :error-function
         n-rates :n-rates rate-eta :rate-eta rate-sigma :rate-sigma
         [n population-size] :population-size reward-transformation :reward-transformation
         [low-rate high-rate] :rate-interval} argmap
        {indices :indices groups :groups :as selection-distribution} (selection/lexicase-distribution population population-size :mapper mapper)
        dist (or dist selection-distribution)
        error-classes (keys groups)
        parents (map #(rand-nth (get groups (nth error-classes %))) indices)
        parents-per-rate (int (/ (count population) n-rates))
        parent-rates (mapv (fn [p r] [r (mapv #(assoc % :umad-rate r) p)])
                           (partition parents-per-rate (shuffle parents))
                           rates)
        new-individuals (into []
                              (partition parents-per-rate
                                         (mapper (partial error-function argmap (:training-data argmap))
                                                 (mapcat (fn [[_ p]]
                                                           (mapv #(variation/mutate-gesmr % argmap) p))
                                                         parent-rates))))
        reward-fn (get reward-transformations reward-transformation)
        rewards (mapper (fn [[_ p] i]
                          (apply max (mapv #(reward-fn %2 %1 dist) p i)))
                        parent-rates
                        new-individuals)
        [elite non-elite] (selection/meta-selection-gesmr rewards rate-eta)
        rates (into [] (concat [(nth rates elite)]
                               (map #(variation/clamp (variation/meta-mutate-gesmr (nth rates %) rate-sigma) :low low-rate :high high-rate)
                                    non-elite)))]
    {:evaluated-pop (into [] (mapcat identity new-individuals))
     :rates rates
     :report-dist (assoc (dissoc selection-distribution :cases :indices)
                         :rewards (double-array rewards)
                         :rates (double-array rates)
                         :groups (into {} (mapv (fn [[k v]] [k (count v)]) groups)))
     :dist selection-distribution}))


  (defn update-info
    ([info dist generation]
     (let [{cur-dist :dist
            gen-per-update :generations-per-update
            dt :dt
            prev-t :t} info
           cur-t (java.lang.System/currentTimeMillis)
           dist (if (zero? (mod generation gen-per-update)) dist cur-dist)]
       (assoc info
              :t cur-t
              :dist dist
              :dt (conj dt (- cur-t prev-t)))))
    ([generations-per-update]
     {:dist nil
      :generations-per-update generations-per-update
      :t (java.lang.System/currentTimeMillis)
      :dt []}))


(defn prettify-dist
  [dist]
  (let [{groups :groups probs :probabilities rew :rewards parameter-rewards :parameter-rewards parameters :parameters} dist]
    (merge (dissoc dist :groups)
           {:probabilities (into {} (mapv #(vector (first %) (double (second %))) probs))
            :parameters (double-array parameters)
            :rewards (double-array rew)})))

(defn custom-report
  "Reports information for each generation."
  [pop generation argmap & {:keys [time new-pop rates dist]}]
  ;; (pprint/pprint (double-array (:rewards dist)))
  (let [best (apply min-key :total-error (concat pop new-pop))]
    (pprint/pprint (merge
                    {:generation            generation
                     :best-program          (genome/plushy->push (:plushy best) argmap)
                     :best-total-error      (:total-error best)
                     :best-errors           (object-array (:errors best))
                     :genotypic-diversity   (float (/ (count (distinct (map :plushy pop))) (count pop)))
                     :behavioral-diversity  (float (/ (count (distinct (map :behaviors pop))) (count pop)))
                     :average-genome-length (float (/ (reduce + (map count (map :plushy pop))) (count pop)))
                     :average-total-error   (float (/ (reduce + (map :total-error pop)) (count pop)))
                    }
                    (when time {:time time})
                    (when dist {:dist (prettify-dist dist)})
                    (when rates {:rates (double-array rates)})))
    (println)))

(defn gp
  "Main GP loop.

   On each iteration, it creates a population of random plushies using a mapper
   function and genome/make-random-plushy function,
   then it sorts the population by the total error using the error-function
   and sort-by function. It then takes the best individual from the sorted population,
   and if the parent selection is set to epsilon-lexicase, it adds the epsilons to the argmap.

   The function then checks if the custom-report argument is set,
   if so it calls that function passing the evaluated population,
   current generation and argmap. If not, it calls the report function
   passing the evaluated population, current generation and argmap.

   Then, it checks if the total error of the best individual is less than or equal
   to the solution-error-threshold or if the current generation is greater than or
   equal to the max-generations specified. If either is true, the function
   exits with the best individual or nil. If not, it creates new individuals
   for the next generation using the variation/new-individual function and the
   repeatedly function, and then continues to the next iteration of the loop. "
  [argmap]
  (let [argmap (make-default argmap)
        _ (do (gp/print-argmap {:starting-args argmap})
              (println))
        {population-size :population-size
         report-interval :report-interval
         gens-per-update :generations-per-update
         mapper :mapper
         instructions :instructions 
         max-initial-plushy-size :max-initial-plushy-size
         solution-error-threshold :solution-error-threshold
         max-generations :max-generations
         error-function :error-function 
         n-rates :n-rates} argmap
        rates (make-mutation-rates n-rates)
        init-pop (mapper
                  (fn [_] {:plushy (genome/make-random-plushy instructions max-initial-plushy-size)})
                  (range (second population-size)))]
    (loop [generation 0
           population (identity (mapper
                                 (partial error-function argmap (:training-data argmap))
                                 init-pop))
           rates rates
           info (update-info gens-per-update)]
      (let [argmap (if-let [downsample-rate (:downsampling-rate argmap)]
                     (let [num-cases (count (:training-data argmap))]
                       (assoc argmap :downsampled-cases (take (int (* num-cases downsample-rate)) (shuffle (range num-cases)))))
                     argmap)
            population (if (#{:plexicase :epsilon-plexicase} (:parent-selection argmap)) (selection/assign-probabilities population argmap) population)
            best-individual (apply min-key :total-error population)
            argmap (condp = (:parent-selection argmap)
                     :epsilon-lexicase (assoc argmap :epsilons (selection/epsilon-list population))
                     :plexicase (selection/reduction-probs population argmap)
                     :epsilon-plexicase (selection/reduction-probs population argmap)
                     argmap)]
        (cond
          ;; Success on training cases is verified on testing cases
          (<= (:total-error best-individual) solution-error-threshold)
          (if-let [best-ind (first
                             (filter #(<= (:total-error %) solution-error-threshold)
                                     (mapper (partial error-function (dissoc argmap :downsampled-cases) (:training-data argmap))
                                             (filter #(<= (:total-error %) solution-error-threshold) population))))]
            (finish-gp generation best-ind true argmap)
            (let [{new-pop :new-pop evaluated-pop :evaluated-pop rates :rates dist :dist report-dist :report-dist} (step-generation population rates argmap :dist (:dist into))]
              (when (zero? (mod generation report-interval))
                (custom-report evaluated-pop generation argmap :new-pop new-pop :time (- (java.lang.System/currentTimeMillis) (:t info)) :dist report-dist :rates rates))
              (recur (inc generation)
                     evaluated-pop
                     rates
                     (update-info info dist generation))))
          ;;
          (>= generation max-generations) (finish-gp generation best-individual false argmap)
          ;;
          :else (let [{new-pop :new-pop evaluated-pop :evaluated-pop rates :rates dist :dist report-dist :report-dist} (step-generation population rates argmap :dist (:dist info))]
                  (when (zero? (mod generation report-interval))
                    (custom-report evaluated-pop generation argmap :new-pop new-pop :time (- (java.lang.System/currentTimeMillis) (:t info)) :dist report-dist :rates rates))
                  (recur (inc generation)
                         evaluated-pop
                         rates
                         (update-info info dist generation))))))))
