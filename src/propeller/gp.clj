(ns propeller.gp
  "Main genetic programming loop."
  (:require [clojure.string]
            [clojure.pprint :as pprint]
            [propeller.genome :as genome]
            [propeller.simplification :as simplification]
            [propeller.variation :as variation]
            [propeller.push.instructions.bool]
            [propeller.push.instructions.character]
            [propeller.push.instructions.code]
            [propeller.push.instructions.input-output]
            [propeller.push.instructions.numeric]
            [propeller.push.instructions.polymorphic]
            [propeller.push.instructions.string]
            [propeller.push.instructions.vector]
            [propeller.selection :as selection]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.tools.math :as math]
            [propeller.tools.metrics :as metrics]
            [propeller.tools.concurrent :as concurrent]
            [propeller.tools.distributions :as dist]
            [clojure.set :as set])
  (:import java.lang.Thread))

(defn make-empty 
  "Makes an empty version of x, accounting for converting map entries to vectors"
  [x]
  (if (map-entry? x) [] 
      (empty x)))

(defn recursive-string 
  "Recursively converts items in x to strings, when not collections, keywords, or numbers."
  [x]
  (cond 
    (coll? x) (into (make-empty x) (map recursive-string x))
    (keyword? x) x
    (number? x) x
    (let [c (class x)] (and c (.isArray c))) x
    :else (str x)))

(defn print-argmap
  "Prints the argmap, making functions into strings to be safe for reading in as a clojure datastructure"
  [argmap]
  (pprint/pprint (into {} (map #(vector (first %) (recursive-string (second %)))
                                 (update argmap :starting-args dissoc :training-data :testing-data :instructions)))))


(defn default-input
  [input & {:keys [single-input?]}]
  (if single-input?
    {:in1 input}
    (into {}
          (map-indexed
           #(vector (keyword (str "in" (inc %1))) %2)
           input))))

(defn default-error-function
  [& {:keys [input-fn initialized-stacks out-stacks error proxy-error]
      :or {input-fn default-input
           out-stacks :print
           error metrics/levenshtein-distance}}]
  (fn [argmap data individual]
    (let [;; {mapper :mapper} argmap
          mapper mapv
          program (genome/plushy->push (:plushy individual) argmap)
          cases (or (:downsampled-cases argmap) (range (count data)))
          inputs (map (comp #(first (if (map? %) (:data %) %)) (partial nth data)) cases)
          targets (map (comp #(second (if (map? %) (:data %) %)) (partial nth data)) cases)
          outputs ((if (coll? out-stacks) mapper #(mapcat identity (mapper %1 %2)))
                   (fn [input]
                     (let [stack (interpreter/interpret-program
                                  program
                                  (merge
                                   state/empty-state
                                   {:input (input-fn input)}
                                   initialized-stacks)
                                  (:step-limit argmap))
                           out (mapv (partial state/peek-stack stack)
                                     (if (coll? out-stacks) out-stacks [out-stacks]))]
                      ;;  (pprint/pprint stack)
                      ;;  (pprint/pprint out)
                       out))
                   inputs)
          errors (flatten (map error
                               targets
                               outputs))]
      ;; (when (< (apply + errors) 0) (pprint/pprint {:proxy proxy-error :error errors :total (apply + errors)}))
      (merge individual
             {:behaviors outputs}
             (if (nil? proxy-error)
               {:errors errors
                :total-error (apply +' errors)}
               (let [proxy (flatten (proxy-error targets outputs errors))]
                 {:original-errors errors
                  :errors proxy
                  :total-error (apply +' proxy)}))
             (when (:inherit-depth argmap)
               {:inherited-errors (conj (get individual :inherited-errors []) errors)})))))


(defn proxy-duplicate
  "Given the frequencies of each test case, duplicates them to match the 
   frequencies.
   Alternatively, given a set of cases, a duplication number, and the total number of 
   test cases, duplicates the cases in the given set by the duplication number"
  ([frequencies]
  (fn [_ _ errors]
    (map #(repeat %1 %2) 
         frequencies 
         errors)))
  ([cases n num-cases]
   (let [cases (set cases)]
    (proxy-duplicate (map #(if (cases %) n 1) 
                          (range num-cases))))))

(defn proxy-weighted
  "Given a mxn set of weights, uses them to construct m proxy-error functions as 
   linear combinations of n test cases. 
   Alternatively samples the mxn set of weights from a distribution"
  ([weights]
   (fn [_ _ errors]
     (map #(math/dot % errors) weights)))
  ([num-proxy distribution num-cases std]
   (proxy-weighted (repeatedly num-proxy 
                               #(math/softmax (dist/get-distribution distribution num-cases std 0))))))

(defn proxy-agg
  "Given a list of groups of test cases and an aggregating function, 
   applies that aggregating function to the errors of each group of cases"
  [case-groups agg]
  (fn [_ _ errors]
    (map (fn [group] 
           (agg (map #(nth errors %) group))) 
         case-groups)))



(defn report
  "Reports information for each generation."
  [pop generation argmap & {:keys [time]}]
  (let [best (apply min-key :total-error pop)]
    (pprint/pprint (merge 
                    {:generation            generation
                    :best-plushy           (:plushy best)
                    :best-program          (genome/plushy->push (:plushy best) argmap)
                    :best-total-error      (:total-error best)
                    :best-errors           (:errors best)
                    :best-behaviors        (:behaviors best)
                    :genotypic-diversity   (float (/ (count (distinct (map :plushy pop))) (count pop)))
                    :behavioral-diversity  (float (/ (count (distinct (map :behaviors pop))) (count pop)))
                    :average-genome-length (float (/ (reduce + (map count (map :plushy pop))) (count pop)))
                    :average-total-error   (float (/ (reduce + (map :total-error pop)) (count pop)))}
                    (when (:original-errors best)
                      {:best-original-errors (:original-errors best)
                       :best-total-original-error (reduce + (:original-errors best))})
                    (when-let [i (:test-error-interval argmap)]
                      (when (= (dec i) (mod generation i)) 
                        (let [{error-function :error-function 
                               testing-data :testing-data} argmap
                              test-errors (error-function argmap testing-data best)]
                          {:test-errors test-errors 
                           :total-test-error (reduce + test-errors)})))
                    (when-let [t time]
                      {:time t})))
    (println)))

(defn concise-report
  "Reports information for each generation."
  [pop generation argmap]
  (println {:generation generation
  :best-total-error (apply min (map :total-error pop))}))


(defn make-default
  [argmap]
  (merge {:solution-error-threshold 0 
          :restart 0
          :mapper concurrent/service-mapper} 
         (update argmap
                       :population-size
                       #(if (number? %) [% %] %))))

(defn report-success 
  [generation individual error-function argmap]
  (prn {:success-generation generation})
  (prn {:total-test-error
        ((fn [e] (if-let [{orig :original-errors} e] (reduce + orig) (:total-error e))) 
         (error-function argmap (:testing-data argmap) individual))})
  (when (:simplification? argmap)
    (let [simplified-plushy (simplification/auto-simplify-plushy (:plushy individual) error-function argmap)]
      (prn {:total-test-error-simplified ((fn [e] (if-let [{orig :original-errors} e] (reduce + orig) (:total-error e)))
                                          (error-function argmap (:testing-data argmap) {:plushy simplified-plushy}))
            :simplified-plushy simplified-plushy
            :simplified-program (genome/plushy->push simplified-plushy argmap)}))))


#_(defn gp-single
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
           max-initial-plushy-size solution-error-threshold mapper restart simplification-steps simplification-k]
    :or   {solution-error-threshold 0.0
           restart 0
           simplification-steps 100
           simplification-k 20
           ;; The `mapper` will perform a `map`-like operation to apply a function to every individual
           ;; in the population. The default is `map` but other options include `mapv`, or `pmap`.
           mapper concurrent/service-mapper-single}
    :as   argmap}]
  ;;
  (print-argmap {:starting-args argmap})
  (println)
  ;;
  (let [init-pop (mapper
                  (fn [_] {:plushy (genome/make-random-plushy instructions max-initial-plushy-size)})
                  (range (second population-size)))]
    (loop [generation 0
         population (mapper
                  (partial error-function argmap (:training-data argmap))
                  init-pop)]             ;creates population of random plushys
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
                   argmap)
          {indices :indices groups :groups} (selection/lexicase-distribution population (second population-size))
           error-classes (keys groups)
           parents (map #(rand-nth (get groups (nth error-classes %))) indices)
          rand-ind (fn [] {:plushy (genome/make-random-plushy instructions max-initial-plushy-size)})
          new-ind #(if (< (rand) restart) (rand-ind) (variation/new-individual [%] argmap :preselected? true))]
      (cond
          ;; Success on training cases is verified on testing cases
        (<= (:total-error best-individual) solution-error-threshold)
        (if-let [best-ind (first
                           (filter #(<= (:total-error %) solution-error-threshold)
                                   (mapper (partial error-function (dissoc argmap :downsampled-cases) (:training-data argmap))
                                           (filter #(<= (:total-error %) solution-error-threshold) population))))]
          (report-success generation best-ind error-function argmap)
          (let [new-pop (mapper new-ind parents)
                new-pop (mapper
                         (partial error-function argmap (:training-data argmap))
                         new-pop)
                evaluated-pop (take (first population-size)
                                    (concat new-pop
                                            population))]
            (if (:custom-report argmap)
              ((:custom-report argmap) evaluated-pop generation argmap)
              (report evaluated-pop generation argmap))
            (recur (inc generation)
                   evaluated-pop)))
          ;;
        (>= generation max-generations)
        {:success? false
         :generation generation
         :min-error (:total-error best-individual)}
          ;;
        :else (let [new-pop (mapper new-ind parents)
                    new-pop (mapper
                             (partial error-function argmap (:training-data argmap))
                             new-pop)
                    evaluated-pop (take (first population-size)
                                        (concat new-pop
                                                population))]
                (if (:custom-report argmap)
                  ((:custom-report argmap) evaluated-pop generation argmap)
                  (report evaluated-pop generation argmap))
                (recur (inc generation)
                     evaluated-pop))))))
  #_(shutdown-agents))


(defn gp-single
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
           max-initial-plushy-size solution-error-threshold mapper restart simplification-steps simplification-k]
    :or   {solution-error-threshold 0.0
           restart 0
           simplification-steps 100
           simplification-k 20
           ;; The `mapper` will perform a `map`-like operation to apply a function to every individual
           ;; in the population. The default is `map` but other options include `mapv`, or `pmap`.
           mapper concurrent/service-mapper-single}
    :as   argmap}]
  ;;
  (print-argmap {:starting-args argmap})
  (println)
  ;;
  (loop [generation 0
         population []
         new-pop (mapper
                  (fn [_] {:plushy (genome/make-random-plushy instructions max-initial-plushy-size)})
                  (range (second population-size)))
         time (java.lang.System/currentTimeMillis)]             ;creates population of random plushys
    (let [argmap (if-let [downsample-rate (:downsampling-rate argmap)]
                   (let [num-cases (count (:training-data argmap))]
                     (assoc argmap :downsampled-cases (take (int (* num-cases downsample-rate)) (shuffle (range num-cases)))))
                   argmap)
          new-pop (mapper
                   (partial error-function argmap (:training-data argmap))
                   new-pop)
          evaluated-pop (take (first population-size)
                              (concat new-pop
                                      population))            ;population sorted by :total-error
          evaluated-pop (if (#{:plexicase :epsilon-plexicase} (:parent-selection argmap)) (selection/assign-probabilities evaluated-pop argmap) evaluated-pop)
          best-individual (apply min-key :total-error evaluated-pop)
          argmap (condp = (:parent-selection argmap)
                   :epsilon-lexicase (assoc argmap :epsilons (selection/epsilon-list evaluated-pop))
                   :plexicase (selection/reduction-probs evaluated-pop argmap)
                   :epsilon-plexicase (selection/reduction-probs evaluated-pop argmap)
                   argmap)
          rand-ind (fn [] {:plushy (genome/make-random-plushy instructions max-initial-plushy-size)})
          new-ind #(if (< (rand) restart) (rand-ind) (variation/new-individual evaluated-pop argmap))]
      (if (:custom-report argmap)
        ((:custom-report argmap) evaluated-pop generation argmap)
        (report evaluated-pop generation argmap :time (- (java.lang.System/currentTimeMillis) time)))
      (cond
          ;; Success on training cases is verified on testing cases
        (<= (:total-error best-individual) solution-error-threshold)
        (if-let [best-ind (first
                           (filter #(<= (:total-error %) solution-error-threshold)
                                   (mapper (partial error-function (dissoc argmap :downsampled-cases) (:training-data argmap))
                                           (filter #(<= (:total-error %) solution-error-threshold) evaluated-pop))))]
          (report-success generation best-ind error-function argmap)
          (recur (inc generation)
                 evaluated-pop
                 (mapper (fn [_] (new-ind)) (range (second population-size)))
                 (java.lang.System/currentTimeMillis)))
          ;;
        (>= generation max-generations)
        {:success? false
         :generation generation
         :min-error (:total-error best-individual)}
          ;;
        :else (recur (inc generation)
                     evaluated-pop
                     (mapper (fn [_] (new-ind)) (range (second population-size)))
                     (java.lang.System/currentTimeMillis)))))
  #_(shutdown-agents))


(defn evaluate 
  [population argmap]
  (sort-by :total-error
           ((:mapper argmap)
            (partial (:error-function argmap) argmap (:training-data argmap))
            population)))

(defn print-return 
  [x]
  (println x)
  x)

;;Should I implement elitism?
(defn next-generation
  [evaluated-pop argmap]
  (let [{instructions :instructions
         mapper :mapper
         max-initial-plushy-size :max-initial-plushy-size
         population-size :population-size
         restart :restart} argmap
        rand-ind (fn [_] {:plushy (genome/make-random-plushy instructions
                                                             max-initial-plushy-size)})
        new-ind (fn [_] (if (< (rand) restart)
                          (rand-ind nil)
                          (variation/new-individual evaluated-pop argmap)))]
    (-> (if (nil? evaluated-pop)
          (mapper rand-ind (range (second population-size)))
          (mapper new-ind (range (second population-size))))
        (evaluate argmap)
        (concat evaluated-pop)
        ((partial take (first population-size))))))

(defn simplify
  [best-individual argmap]
  (let [simplified-plushy (simplification/auto-simplify-plushy (:plushy best-individual) (:error-function argmap) argmap)]
    {:total-test-error-simplified (:total-error ((:error-function argmap) argmap (:testing-data argmap) (hash-map :plushy simplified-plushy)))}))

(defn report-generation
  [out-file evaluated-pop generation argmap]
  (spit out-file
        (with-out-str ((or (:custom-report argmap)
                           report) evaluated-pop
                                   generation
                                   argmap))
        :append true))

(defn step-generation
  [generation]
  (let [{gen :generation
         population :population 
         argmap :argmap
         success :success
         test-error :test-error} generation
        {solution-error-threshold :solution-error-threshold
         simplification? :simplification?
         max-generations :max-generations
         error-function :error-function} argmap]
    (if (or success (>= gen max-generations)) 
      generation 
      (let [best-individual (apply min-key :total-error population)
            argmap (if (= (:parent-selection argmap) :epsilon-lexicase)
                     (assoc argmap :epsilons (selection/epsilon-list population))
                     argmap)
            success (<= (:total-error best-individual) solution-error-threshold)
            test-error (if success 
                         (:total-error (error-function argmap (:testing-data argmap) best-individual))
                           test-error)]
        (when-let [o (:out-file generation)] 
          (report-generation o population gen argmap)
          (when success (spit o (with-out-str (println {:success-generation gen}) (println {:test-error test-error}))
                              :append true)))
        (merge generation
               {:generation (inc gen)
                :best-individual best-individual
                :population (next-generation population argmap)
                :success success
                :test-error test-error}
               (when (and success simplification?) (simplify best-individual argmap)))))))

(defn step-with-callback 
  [generation callback]
  (let [{central-agent :central-agent
         success :success 
         num-gen :generation
         {max-generations :max-generations} :argmap
         :as gen} (step-generation generation)]
    (when-not (or success (>= num-gen max-generations)) 
      (send central-agent callback))
    gen))

(defn gp-agent
  [central-agent argmap & {:keys [run out-file]}]
  (let [argmap (update argmap
                       :population-size
                       #(if (number? %) [% %] %))]
    (agent
     (merge
      {:population (next-generation nil argmap)
       :generation 0
       :argmap argmap
       :success false
       :best-individual {:total-error ##-Inf}
       :central-agent central-agent}
      (when run {:run run})
      (when out-file {:out-file out-file})))))

(defn request-new-step 
  [central-agent run]
  (send (nth (:gp-agents central-agent) run) 
        step-with-callback
        #(request-new-step % run))
  central-agent)

(defn run-gp 
  [central-agent argmap]
  (mapv #(mapv (fn [_] (send % step-generation)) 
               (range (:max-generations argmap))) 
        (:gp-agents central-agent))
  central-agent)

(defn run-gp-requests 
  [central-agent argmap]
  (mapv #(send-off central-agent request-new-step %) 
        (range (count (:gp-agents @central-agent)))))

(defn central-agent 
  [num-runs argmap & {:keys [out-file request]}]
  (let [{mapper :mapper} argmap
        central-agent (agent {})
        gp-agents (mapper #(gp-agent central-agent 
                                     argmap
                                     :run %
                                     :out-file (nth out-file %)) 
                          (range num-runs))]
    (send central-agent #(assoc % :gp-agents gp-agents))
    (await central-agent)
    (if request 
      (run-gp-requests central-agent argmap)
      (send central-agent run-gp argmap))
    (await central-agent)
    central-agent))

(defn central-report 
  [central-agent]
  (let [gp-agents (mapv deref (:gp-agents central-agent))
        reports (mapv (fn [{g :generation
                            s :success
                            b :best-individual
                            t :test-error}]
                        {:generation g
                         :success s
                         :best-error (:total-error b)
                         :test-error t})
                      gp-agents)]
    {:reports reports 
     :successes (count (filter :success reports))
     :average-success-generation (math/mean (map :generation (filter :success reports)))
     :average-total-generation (math/mean (map :generation reports))
     :average-best-error (math/mean (map :best-error reports))
     :average-test-error (math/mean (map :test-error (filter :success reports)))}))

(defn run-over? 
  [gp-agent argmap]
  (or (:success gp-agent)
      (= (:max-generations argmap)
         (:generation gp-agent))))

(defn run-experiment
  [num-runs argmap & {:keys [out-file request]}]
  (let [out-file (cond (coll? out-file) out-file 
                       (nil? out-file) nil
                       :else (mapv #(str out-file "/" % ".out") (range num-runs)))
        argmap (make-default argmap)
        master (central-agent num-runs argmap :out-file out-file :request request)]
    (mapv #(spit % (with-out-str (prn {:starting-args (update (update argmap :error-function str) :instructions str)})))
          out-file)
    (loop [end false]
      (if end
        (do 
          (shutdown-agents)
          (pprint/pprint (central-report @master)))
        (do (pprint/pprint (central-report @master))
            (Thread/sleep (or (:report-interval argmap)
                              600000))
            (recur (every? #(run-over? @% argmap)
                           (:gp-agents @master))))))))


(defn gp 
  [argmap]
  (if-let [num-runs (:num-runs argmap)]
    (run-experiment num-runs (make-default argmap) 
                    :out-file (:out-file argmap)
                    :request (:request argmap)) 
    (gp-single (make-default argmap))))

#_(let [num-runs 10 
      out-file "test"]
  (if (coll? out-file) out-file (mapv #(str out-file "/" % ".out") (range num-runs))))

(defn all-min-index
  [coll]
  (let [m (apply min coll)]
    (filter identity (map-indexed #(when (= %2 m) %1) coll))))

(defn gumbel
  []
  (- (Math/log (- (Math/log (rand 1))))))

(defn gumbel-max
  [coll]
  (let [n (count coll)
        shifted (map + coll (repeatedly n gumbel))]
    (apply max-key (partial nth shifted) (range n))))

(defn softmax-ranked
  [coll]
  (let [c (map-indexed #(vector %1 (first %2)) (sort-by second > (map-indexed vector coll)))]
    (second (nth c (gumbel-max (map first c))))))

(defn softmax-log
  [coll]
  (gumbel-max (map #(- (Math/log %)) coll)))

(defn sample
  [agents argmap]
  (let [{sampling-type :sampling-type
         epsilon :epsilon} argmap
        errors (map #(:total-error (:best-individual @%)) agents)]
    (condp = sampling-type
      :epsilon-greedy (if (< (rand) epsilon)
                        (rand-int (count agents))
                        (rand-nth (all-min-index errors)))
      :softmax-log (softmax-log errors)
      :epsilon-softmax-log (if (< (rand) epsilon)
                             (rand-int (count agents))
                             (softmax-log errors))
      :softmax-ranked (softmax-ranked errors)
      :epsilon-softmax-ranked (if (< (rand) epsilon)
                                (rand-int (count agents))
                                (softmax-ranked errors))
      (sample agents (merge argmap {:sampling-type :epsilon-greedy
                                    :epsilon (or (:epsilon argmap) 0.25)})))))

(defn max-gp-agent
  [argmap & {:keys [id out-file]}]
  (agent
   (let [{num-runs :num-runs
          mapper :mapper} argmap
         mapper (or mapper mapv)]
     {:gp-agents (mapper #(deref (gp-agent nil
                                           argmap
                                           :run %))
                         (range num-runs))
      :n (into [] (repeat num-runs 0))
      :generation 0
      :id id
      :out-file out-file
      :best-individual {:total-error ##Inf}
      :test-error ##Inf
      :success false
      :argmap argmap})))

(defn step-max-gp-agent
  [max-gp-agent]
  (let [{out-file :out-file
         gen :generation
         n :n
         success :success
         gp-agents :gp-agents
         {max-generations :max-generations :as argmap} :argmap} max-gp-agent]
    (when out-file
      (spit out-file (with-out-str (println "generation " gen " sampled " n " errors " (map #(:total-error (:best-individual @%)) gp-agents)))))
    (cond success max-gp-agent 
          (= gen max-generations)
          (do (when out-file (spit out-file (with-out-str (println {:success false :generation gen})
                                                          (println {:test-error "NA"}))))
              max-gp-agent)
          (some identity (map #(:success @%) gp-agents))
          (do (when out-file (spit out-file (with-out-str (println {:success true :success-generation gen})
                                                          (println {:test-error "NA"}))))
              (assoc max-gp-agent :success true))
          :else (let [i (sample gp-agents argmap)
                      gp-agents (update gp-agents i step-generation)]
                  (assoc max-gp-agent
                         :gen (inc gen)
                         :gp-agents gp-agents
                         :n (update n i inc)
                         :best-individual {:total-error (apply min (map #(:total-error (:best-individual %)) gp-agents))})))))

(defn max-gp-agentwise
  [argmap]
  (print-argmap argmap)
  (let [{num-experiments :num-experiments
         max-generations :max-generations
         out-file :out-file
         :as argmap} (make-default argmap)
        out-file  (cond (coll? out-file) out-file
                        (nil? out-file) nil
                        :else (mapv #(str out-file "/" % ".out") (range num-experiments)))
        max-gp-agents (mapv #(max-gp-agent argmap :id % :out-file (nth out-file %)) (range num-experiments))]
    #_(mapv #(dotimes [_ max-generations] 
             (send % step-max-gp-agent))
          max-gp-agents)
    (step-max-gp-agent @(first max-gp-agents))
    (println "first step done")
    (await (send (first max-gp-agents) step-max-gp-agent))
    (println "second await done")
    (loop [end false]
      (if end
        (do
          (shutdown-agents)
          (pprint/pprint (central-report {:gp-agents max-gp-agents})))
        (do (pprint/pprint (central-report {:gp-agents max-gp-agents}))
            (Thread/sleep (or (:report-interval argmap)
                              600000))
            (recur (every? #(run-over? @% argmap)
                           max-gp-agents)))))))



(defn max-gp
  [{num-runs :num-runs
    max-generations :max-generations
    epsilon :epsilon
    pbt-generations :pbt-generations
    out-file :out-file
    mapper :mapper
    sampling-type :sampling-type
    population-size :population-size
    :as argmap}]
  (print-argmap argmap)
  (let [pbt-generations (or pbt-generations max-generations)
        mapper (or mapper mapv)
        n (into [] (repeat num-runs 0))
        out-file (cond (coll? out-file) out-file
                       (nil? out-file) nil
                       :else (mapv #(str out-file "/" % ".out") (range num-runs)))
        argmap (make-default argmap)
        gp-agents (mapper #(gp-agent central-agent
                                     argmap
                                     :run %
                                     :out-file (nth out-file %))
                          (range num-runs))]
    (mapv #(spit % (with-out-str (prn {:starting-args (update (update argmap :error-function str) :instructions str)})))
          out-file)
    (loop [gen 0
           n n]
      (println "generation " gen " sampled " n " errors " (map #(:total-error (:best-individual @%)) gp-agents))
      (cond (= gen max-generations)
            (do (println {:success false :generation gen})
                (println {:test-error "NA"})
                (shutdown-agents))
            (some identity (map #(:success @%) gp-agents))
            (do (println {:success true :success-generation gen})
                (println {:test-error "NA"})
                (shutdown-agents))
            :else (let [i (sample gp-agents argmap)]
                    (await (send (nth gp-agents i) step-generation))
                    (when (= (dec pbt-generations)
                             (mod gen pbt-generations))
                      (let [errors (map #(:total-error (:best-individual @%)) gp-agents)
                            best-index (rand-nth (all-min-index errors))
                            best-gen @(nth gp-agents best-index)]
                        (apply await (mapv #(send % (constantly best-gen)) gp-agents))))
                    (recur (inc gen)
                           (update n i inc)))))))

;;I have to implement some kind of steady state GA such as maxbandit here.


