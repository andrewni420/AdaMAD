(ns propeller.problems.regression.function-minimization
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.tools.math :as math]
            [propeller.tools.maxbandit :as maxbandit]
            [propeller.gp :as gp]
            [clojure.pprint :as pprint]
            [propeller.selection :as selection]
            [propeller.tools.concurrent :as concurrent]
            [propeller.variation :as variation]
            [propeller.simplification :as simplification]
            [propeller.tools.distributions :as dist]))


;; Test functions for function minimization from GSEMR paper

(defn linear
  [input]
  (reduce + input))

(defn sphere
  [input]
  (transduce (map math/square) + input))

(defn rosenbrock
  [input]
  (reduce +
          (map #(+ (* 100 (math/square (- %2 (math/square %1))))
                   (math/square (dec %1)))
               input
               (rest input))))

(defn rastrigin
  [input]
  (+ (* 10 (count input))
     (transduce (map #(- (math/square %)
                         (* 10 (math/cos (* 2 math/PI %)))))
                +
                input)))

(defn griewank
  [input]
  (- (/ (transduce (map math/square) + input) 4000)
     (reduce * 1
             (map-indexed #(math/cos (/ %2 (math/sqrt (inc %1))))
                          input))
     (- 1)))

(defn ackley
  [input & {:keys [a b c]
            :or {a 20 b 0.2 c (* 2 math/PI)}}]
  (- (+ a math/E)
     (* a (math/exp (* (- b) (math/sqrt (math/mean (map math/square input))))))
     (math/exp (math/mean (map #(math/cos (* c %)) input)))))

(defn levy 
  [input]
  (let [w (map #(inc (/ (dec %) 4)) input)]
    (+ (math/square (math/sin (* math/PI (first w)))) 
       (reduce + (map #(* (math/square (dec %)) (inc (* 10 (math/square (math/sin (inc (* math/PI %))))))) (drop-last w)))
       (* (math/square (dec (last w))) (inc (math/square (math/sin (* 2 math/PI (last w)))))))))

(defn schwefel 
  [input]
  (- (* 418.9829 (count input))
     (reduce + (map #(* % (math/sin (math/sqrt (math/abs %)))) input))))

(defn make-error-function
  [func]
  (fn [individual]
    (let [err (func (:genome individual))]
      (assoc individual
             :errors [err]
             :behaviors [err]
             :total-error err))))


(def error-functions
  (into {}
        (map (fn [[k v]] [k (make-error-function v)])
             {:linear linear
              :sphere sphere
              :rosenbrock rosenbrock
              :rastrigin rastrigin
              :griewank griewank
              :ackley ackley
              :levy levy
              :schwefel schwefel})))

(defn make-individual
  [argmap]
  (let [{dim :dim
         std :std} argmap]
    {:genome (into [] (dist/rand-norm {:n dim :mu 0 :sigma std}))}))


(defn mutate
  "Create a new individual using supplied parameters"
  [ind parameters]
  (let [{e :e
         meta-std :meta-std} parameters
        meta-std (or meta-std 2)
        {ind-std :std
         genome :genome} ind
        std (or ind-std e)]
    (merge
     {:genome (mapv + genome (dist/rand-norm {:n (count genome) :mu 0 :sigma std}))
      :bandit-parameters {:e e}}
     (when ind-std {:std (variation/meta-mutate-gsemr ind-std meta-std)}))))





(defn step-generation
  "Normal step generation with elitism and truncation selection"
  [population argmap]
  (let [{mapper :mapper error-function :error-function sigma :sigma
         [n population-size] :population-size} argmap
        parents (mapper (fn [_] (selection/truncation-selection population argmap)) (range (dec population-size)))
        new-pop (mapper #(mutate % {:e sigma}) parents)
        evaluated-pop (mapper error-function new-pop)]
    {:evaluated-pop (conj evaluated-pop (apply min-key :total-error population))}))

(defn look-ahead
  "Look ahead a number of generations and return the best error found"
  [population generations argmap]
  (let [{mapper :mapper error-function :error-function
         [n population-size] :population-size reward-transformation :reward-transformation} argmap
        parents (conj (repeatedly (dec population-size) #(selection/truncation-selection population argmap)))]
    (loop [population population
           g 0
           best-error ##Inf]
      (if (= g generations)
        best-error
        (let [{pop :evaluated-pop} (step-generation population argmap)]
          (recur pop
                 (inc g)
                 (min best-error (apply min (map :total-error pop)))))))))


(defn step-generation-gsemr
  [population rates argmap]
  (let [{mapper :mapper error-function :error-function
         n-rates :n-rates rate-eta :rate-eta rate-sigma :rate-sigma
         [n population-size] :population-size reward-transformation :reward-transformation} argmap
        parents (conj (repeatedly (dec population-size) #(selection/truncation-selection population argmap)))
        parents-per-rate (int (/ (count population) n-rates))
        parent-rates (mapv (fn [p r] [r (mapv #(assoc % :std r) p)])
                           (partition parents-per-rate (shuffle parents))
                           rates)
        new-individuals (into []
                              (partition parents-per-rate
                                         (mapper error-function
                                                 (mapcat (fn [[_ p]]
                                                           (mapv #(mutate % nil) p))
                                                         parent-rates))))
        reward-fn (get maxbandit/reward-transformations reward-transformation)
        rewards (mapper (fn [[_ p] i]
                          (apply max (mapv #(reward-fn %2 %1 nil) p i)))
                        parent-rates
                        new-individuals)
        [elite non-elite] (selection/meta-selection-gsemr rewards rate-eta)
        rates (into [] (concat [(nth rates elite)]
                               (map #(variation/meta-mutate-gsemr (nth rates %) rate-sigma)
                                    non-elite)))]
    {:evaluated-pop (into [] (mapcat identity (concat [[(apply min-key :total-error population)]] new-individuals)))
     :rates rates}))

(defn step-generation-bandit
  [population bandits argmap]
  (let [{mapper :mapper error-function :error-function
         parameter-override :parameter-override update-freq :update-freq
         [n population-size] :population-size [forward backward] :transformations
         reward-transformation :reward-transformation} argmap
        ;; _ (println "selection time")
        update-freq (if parameter-override population-size update-freq)
        parents (repeatedly (dec population-size) #(selection/truncation-selection population argmap))
        new-ind (fn [bandit-ensemble] #(mutate %
                                               (or (if (fn? parameter-override) (parameter-override) parameter-override)
                                                   (maxbandit/sample-bandit-ensemble bandit-ensemble :transformation forward))))
        parents (partition update-freq update-freq [] parents)]
    (loop [parents parents
           bandits bandits
           new-pop []
           total-rewards []]
      (if (empty? parents)
        (let [pop (into []
                        (take n)
                        (concat [(apply min-key :total-error population)]
                                new-pop
                                population))]
          (pprint/pprint {:mean-rate (math/mean (into [] (filter identity (map #(:e (:bandit-parameters %)) pop))))})
          {:evaluated-pop pop
           :rates (into [] (filter identity (map #(:e (:bandit-parameters %)) pop)))
           :bandits bandits})
        (let [;;_ (println "creation time")
              add-pop (identity (mapper (new-ind bandits) (first parents)))
              ;; _ (println "evaluation time")
              add-pop (identity (mapper error-function add-pop))
              ;; _ (println "reward time")
              transform (get maxbandit/reward-transformations reward-transformation)
              rewards (identity (mapper #(transform %1 %2 nil)
                                        add-pop
                                        (first parents)))
              bandits (identity (if parameter-override
                                  bandits
                                  (maxbandit/update-bandit-ensemble (mapv :bandit-parameters add-pop)
                                                                    rewards
                                                                    bandits
                                                                    :mapper mapper
                                                                    :transformation backward
                                                                    :parallel true)))]
          (recur (rest parents)
                 bandits
                 (concat new-pop add-pop)
                 (concat total-rewards rewards)))))))

(defn look-ahead-mutation-rate 
  "Determine best mutation rate via a lookahead search"
  [population argmap]
  (let [{lamr-gen :lamr-gen lamr-rates :lamr-rates} argmap 
        best-errors (mapv #(look-ahead population lamr-gen (assoc argmap :sigma %)) lamr-rates)
        best-idx (apply min-key (partial nth best-errors) (range (count best-errors)))]
    (nth lamr-rates best-idx)))

(defn step-generation-samr
  "Normal step generation with elitism and truncation selection"
  [population argmap]
  (let [{mapper :mapper error-function :error-function meta-std :meta-std
         [n population-size] :population-size} argmap
        parents (mapper (fn [_] (selection/truncation-selection population argmap)) (range (dec population-size)))
        new-pop (mapper #(mutate % {:meta-std meta-std}) parents)
        evaluated-pop (mapper error-function new-pop)]
    {:evaluated-pop (conj evaluated-pop (apply min-key :total-error population))}))

(defn finish-gp
  [generation best-ind bandits success argmap]
  (let [{error-function :error-function} argmap]
    (maxbandit/print-bandits bandits)
    (if success
      (prn {:success-generation generation})
      (do (prn {:success false
                :total-test-error ##Inf
                :generation generation
                :min-error (:total-error best-ind)})
          {:success? false
           :generation generation
           :min-error (:total-error best-ind)}))
    (shutdown-agents)))

(defn update-info
  ([info]
   (let [{dt :dt
          prev-t :t} info
         cur-t (java.lang.System/currentTimeMillis)]
     (assoc info
            :t cur-t
            :dt (conj dt (- cur-t prev-t)))))
  ([]
   {:dist nil
    :t (java.lang.System/currentTimeMillis)
    :dt []}))


(defn make-default
  [argmap]
  (merge {:solution-error-threshold 0
          :restart 0
          :mapper concurrent/service-mapper
          :transformations [maxbandit/tile-to-parameters-umad maxbandit/parameters-to-tile-umad]
          :error-transformation (maxbandit/get-error-transformation :nlog :array-errors false)
          :update-freq 100
          :report-interval 1
          :reward-transformation :safe-log-diff
          :generations-per-update 1
          :initializer (fn [_] (make-individual {:dim (:dim argmap) :std (:std argmap)}))}
         (update argmap
                 :population-size
                 #(if (number? %) [% %] %))))

(defn make-mutation-rates
  "Make mutation rates for the group elites paper"
  [n-rates]
  (mapv #(math/pow 10 (- (/ (* 6 %) (dec n-rates)) 3)) (range n-rates)))


(defn custom-report
  "Reports information for each generation."
  [pop generation argmap & {:keys [t new-pop bandits dist succinct rates]}]
  ;; (pprint/pprint (first pop))
  (let [best (apply min-key :total-error (concat pop new-pop))]
    (identity (pprint/pprint (merge
                              {:generation            generation}
                              (when-not succinct {:best-program          (genome/plushy->push (:plushy best) argmap)})
                              {:best-total-error      (:total-error best)
                               :best-errors           (object-array (:errors best))
                               :genotypic-diversity   (double (/ (count (distinct (map :plushy pop))) (count pop)))
                               :behavioral-diversity  (double (/ (count (distinct (map :behaviors pop))) (count pop)))
                               :average-genome-length (double (/ (reduce + (map count (map :plushy pop))) (count pop)))
                               :average-total-error   (double (/ (reduce + (map :total-error pop)) (count pop)))}
                              (when t {:time t})
                              (when-let [s (:sigma argmap)] {:sigma s}))))
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
  [{:keys [population-size max-generations error-function instructions
           max-initial-plushy-size solution-error-threshold mapper bandit-parameters n parameter-override
           transformations error-transformation generations-per-update initializer]
    :or   {solution-error-threshold 0.0
           transformations [maxbandit/tile-to-parameters-0 maxbandit/parameters-to-tile-0]
           mapper concurrent/service-mapper
           generations-per-update 1
           error-transformation (maxbandit/get-error-transformation :nlog :array-errors false)}
    :as   argmap}]
  (let [argmap (make-default argmap)
        _ (do (gp/print-argmap {:starting-args argmap})
              (println))
        {population-size :population-size
         report-interval :report-interval
         initializer :initializer
         n-rates :n-rates
         lamr-gen :lamr-gen
         method :method} argmap
        init-pop (mapper
                  initializer
                  (range (second population-size)))
        init-pop (if (= method :samr)
                   (mapv #(assoc %1 :std %2)
                         init-pop
                         (make-mutation-rates (count init-pop)))
                   init-pop)
        bandits (if parameter-override nil
                    (mapv #(maxbandit/evaluate-bandit % :mapper mapper)
                          (apply maxbandit/bandit-ensemble
                                 (into [] (mapcat identity
                                                  bandit-parameters)))))
        rates (when (= :gsemr method) (make-mutation-rates n-rates))]
    (loop [generation 0
           population (identity (mapper error-function init-pop))
           bandits bandits
           rates rates
           argmap argmap
           info (update-info)]
      (let [best-individual (apply min-key :total-error population)]
        (cond
          ;;
          (>= generation max-generations) (finish-gp generation best-individual bandits false argmap)
          ;;
          :else (let [argmap (if (and (= method :lamr) (zero? (mod generation lamr-gen)))
                               (assoc argmap :sigma (look-ahead-mutation-rate population argmap))
                               argmap)
                      {new-pop :new-pop evaluated-pop :evaluated-pop bandits :bandits rates :rates}
                      (condp = method
                        :gsemr (step-generation-gsemr population rates argmap)
                        :bandit (step-generation-bandit population bandits argmap)
                        :lamr (step-generation population argmap)
                        :samr (step-generation-samr population argmap))]
                 (pprint/pprint {:step-generation (apply min (map :total-error evaluated-pop))})
                  (when (zero? (mod generation report-interval))
                    (maxbandit/custom-report evaluated-pop generation argmap
                                            ;;  :new-pop new-pop
                                             :t (- (java.lang.System/currentTimeMillis) (:t info))
                                             :bandits []
                                             :rates (if (= method :samr)
                                                      (double-array (map :std evaluated-pop))
                                                      rates)
                                             :succinct true))
                  (recur (inc generation)
                         evaluated-pop
                         (mapv #(assoc % :generation (inc generation)) bandits)
                         rates
                         argmap
                         (update-info info))))))))

(defn run
  [argmap]
  (gp
   (merge
    {:max-generations         100
     :population-size         [101 101]
     :parent-selection        :truncation-selection
     :truncation-size 10
     :std 1 
     :dim 10
     :n-rates 10
     :rate-sigma 2 
     :rate-eta 4
     :meta-std 2 
     :lamr-gen 100 
     :lamr-rates (mapv #(math/pow 10 (- (/ % 3) 3)) (range 10))
     :update-freq 100
     :report-interval 1
     :method :bandit
     :transformations [maxbandit/tile-to-parameters-umad maxbandit/parameters-to-tile-umad]
     :bandit-parameters {:num-bandits 5
                         :l [-100]
                         :r [100]
                         :to [[0 0.03 0.06 0.09 0.12 0.15]] #_[[0 0.03 0.06 0.09 0.12 0.15 0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39 0.42 0.45 0.48]]
                         :ta [[0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39]] #_[[0.51 0.54 0.57 0.6 0.63 0.66 0.69 0.72 0.75 0.78 0.81 0.84 0.87 0.9]]
                         :num-codings 20
                         :d [1]
                         :acc [0.03]
                         :lr (into [] (map (fn [lr] {:method :momentum :lr (math/pow 10 (- (/ lr 10) 4)) :dampening 1 :momentum-lr 0.1})) (range 10))  
                         #_[{:method :momentum
                               :momentum-lr 0.1
                               :lr 0.0001
                               :dampening 1
                              :alpha 0.0001
                               :beta1 0.1
                               :beta2 0.001}]
                         :s [50]
                         :delta [0.01]
                         :n 100
                         :mode [:argmax]
                         :AO true
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 3000 0.01)))]
                         :maximize? false
                         :max-n 100
                         :sigma 3}
     :generations-per-update 1
     :reward-transformation :diff
    ;;  :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.0001} {:e 0.0003} {:e 0.001} {:e 0.003} {:e 0.01} {:e 0.03} {:e 0.1}]
     :custom-report maxbandit/custom-report
     :elitism                 false}
    argmap)))


(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (let [{problem :problem
         :as argmap} (apply hash-map (map #(if (string? %) (read-string %) %) args))]
    (run (merge {:error-function (get error-functions problem)}
                argmap))))


;; gsemr -> rates 
;; lamr -> sigma 
;; samr -> rates 
;; bandit -> :w or :N



(defn vector-to-string
  [v & {:keys [callback] :or {callback identity}}]
  (apply str "[" (-> v
                     (#(map callback %))
                     (interleave (repeat ", "))
                     (drop-last)
                     (concat ["]"]))))

(defn map-to-string
  [m & {:keys [callback] :or {callback identity}}]
  (apply str "{" (->> m
                      (map (fn [[k v]] [(callback k) ": " (callback v)]))
                      (interleave (repeat ", "))
                      (drop 1)
                      (mapcat identity)
                      (#(concat % ["}"])))))

(defn jsonify
  [x]
  (cond
    (number? x) x
    (string? x) (str "\"" x "\"")
    (keyword? x) (str "\"" x "\"")
    (map? x) (map-to-string x :callback jsonify)
    (coll? x) (vector-to-string x :callback jsonify)
    (.isArray (class x)) (vector-to-string x :callback jsonify)
    :else x))

(defn re-test 
  []
(let [s "linear-gsemr-std1-dim100-187727"]
  (pprint/pprint {:match (re-matches #"linear-gsemr-std1-dim100.*" s)})))

(defn find-file 
  [dir regex]
  ;; (pprint/pprint {:dir dir :regex regex})
  (let [d (file-seq (clojure.java.io/file dir))
        ;; _ (pprint/pprint {:files (mapv #(.getName %) d)})
        f (filter #(and (not (.isFile %)) (re-matches regex (.getName %))) d)]
    (.getName (first f))))

(defn process-bandit
  [filename]
  (let [f (read-string (str "[" (slurp (str filename ".out")) "]"))
        ;; w (into [] (filter identity (map #(->> % (:bandits) (first) (:base-coding) (:w)) f)))
        ;; N (into [] (filter identity (map #(->> % (:bandits) (first) (:base-coding) (:N)) f)))
        _ (pprint/pprint "read file")
        _ (pprint/pprint filename)
        err (into [] (filter identity (map :best-total-error f)))
        out (into [] (filter identity (map :rates f)))
        out (with-out-str (println (jsonify out)))
        err (with-out-str (println (jsonify err)))
        ;; w (with-out-str (println (jsonify (identity w))))
        ;; N (with-out-str (println (jsonify (identity N))))
        ]
    ;; (spit (str filename ".w") w)
    ;; (spit (str filename ".N") N)
    (spit (str filename ".errs") err)
    (spit (str filename ".analysis") out)))



(defn process-rates
  [filename]
  (let [f (read-string (str "[" (slurp (str filename ".out")) "]"))
        out (into [] (filter identity (map :rates f)))
        err (into [] (filter identity (map :best-total-error f)))
         err (with-out-str (println (jsonify err)))
        output (with-out-str (println (jsonify out)))]
    (spit (str filename ".rates") output)
    (spit (str filename ".errs") err)
    (spit (str filename ".analysis") output)))



(defn process-sigma
  [filename]
  (let [f (read-string (str "[" (slurp (str filename ".out")) "]"))
        out (into [] (filter identity (map :sigma f)))
        err (into [] (filter identity (map :best-total-error f)))
         err (with-out-str (println (jsonify err)))
        output (with-out-str (println (jsonify out)))]
    (spit (str filename ".sigma") output)
    (spit (str filename ".errs") err)
    (spit (str filename ".analysis") output)))

(defn process-files 
  []
  (let [
        problems ["ackley", "griewank", "rastrigin", "rosenbrock", "sphere", "linear"] #_["linear", "sphere", "rosenbrock", "rastrigin", "griewank", "ackley"]
        ;; methods ["bandit3" "gsemr" "lamr" "samr"]
        methods ["bandit3-1"]
        directory "/home/ani24/propeller-master/funcmin-results-ensemble"
        ]
    (doall 
     (for [p problems
           m methods]
       (let [pattern (re-pattern (str p "-" m ".*"))
             filename (find-file directory pattern)
             processor (condp = m
                         "bandit3-1" process-bandit
                         "gsemr" process-rates
                         "lamr" process-sigma
                         "samr" process-rates)]
         (println filename)
         (concurrent/service-mapper #(do (println %) (processor (str directory "/" filename "/" %))) (range 1 25))
         (concurrent/service-mapper #(do (println %) (processor (str directory "/" filename "/" %))) (range 25 51)))))))



