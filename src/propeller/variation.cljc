(ns propeller.variation
  "Propeller includes many kinds of genetic operators to create variation within the population.
  You can specify the rate of the variation genetic operators with the `:variation` map.

# Variation

Propeller includes many kinds of genetic operators to create variation within the population.
You can specify the rate of the variation genetic operators with the `:variation` map.

## Crossover

Crossover genetic operators take two `plushy` representations of Push programs
and exchange genetic material to create a new `plushy`.

| Function                         | Parameters             | Description                                                                                                                                            |
|----------------------------------|------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------|
| `crossover`                      | `plushy-a` `plushy-b`  | Crosses over two individuals using uniform crossover, one Push instruction at a time. Pads shorter one from the end of the list of instructions.       |
| `tail-aligned-crossover`         | `plushy-a` `plushy-b`  | Crosses over two individuals using uniform crossover, one Push instruction at a time. Pads shorter one from the beginning of the list of instructions. |
| `diploid-crossover`              | `plushy-a` `plushy-b`  | Crosses over two individuals using uniform crossover with pairs of Push instructions. Pads shorter one from the end of the list of instructions.       |
| `tail-aligned-diploid-crossover` | `plushy-a` `plushy-b`  | Crosses over two individuals using uniform crossover with pairs of Push instructions. Pads shorter one from the beginning of the list of instructions. |

## Addition, Deletion, Replacement, Flip

Addition, deletion, replacement, and flip genetic operators take a `plushy` and a rate of occurrence to create a new `plushy`.

| Function                                  | Parameters                                 | Description                                                                                                                     |
|-------------------------------------------|--------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------|
| `uniform-addition`                        | `plushy` `instructions` `umad-rate`        | Returns a plushy with new instructions possibly added before or after each existing instruction.                                |
| `uniform-replacement`                     | `plushy` `instructions` `replacement-rate` | Returns a plushy with new instructions possibly replacing existing instructions.                                                |
| `diploid-uniform-silent-replacement`      | `plushy` `instructions` `replacement-rate` | Returns a plushy with new instructions possibly replacing existing instructions, but only among the silent member of each pair. |
| `diploid-uniform-addition`                | `plushy` `instructions` `umad-rate`        | Returns a plushy with new instructions possibly added before or after each existing pair of instructions.                       |
| `uniform-deletion`                        | `plushy` `umad-rate`                       | Randomly deletes instructions from plushy at some rate.                                                                         |
| `diploid-uniform-deletion`                | `plushy` `flip-rate`                       | Randomly flips pairs in a diploid plushy at some rate.                                                                          |

## Uniform Mutation by Addition and Deletion

Uniform Mutation by Addition and Deletion (UMAD) is a uniform mutation operator which
first adds genes with some probability before or after every existing gene and then
deletes random genes from the resulting genome. [It has been found](http://cs.hamilton.edu/~thelmuth/Pubs/2018-GECCO-UMAD.pdf) that UMAD, with relatively
high rates of addition and deletion, results in significant increases
in problem-solving performance on a range of program synthesis
benchmark problems. When you run a problem in Propeller, you can specify the `umad-rate` to determine the frequency
of addition and deletion.

`:umad` in the `:variation` map when running a problem will call `uniform-addition` and `uniform-deletion` with the `umad-rate`.
Since `uniform-addition` and `uniform-deletion` are somewhat stochastic, you can use
`:rumad` to ensure that the actual rates of addition and deletion are equal when mutating a genome.

## New Individual

The function `new-individual` returns a new individual produced by selection and variation of individuals in the population based on the genetic operators provided in the `:variation` map."
  {:doc/format :markdown}
    (:require [propeller.selection :as selection]
      [propeller.utils :as utils]
              [propeller.tools.math :as math]
              [propeller.tools.distributions :as dist]))




(defn crossover
  "Crosses over two individuals using uniform crossover, one Push instruction at a time.
   Pads shorter one from the end of the list of instructions."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    (remove #(= % :crossover-padding)
            (map #(if (< (rand) 0.5) %1 %2)
                 shorter-padded
                 longer))))

(defn tail-aligned-crossover
  "Crosses over two individuals using uniform crossover, one Push instruction at a time.
   Pads shorter one from the beginning of the list of instructions."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat (repeat length-diff :crossover-padding) shorter)]
    (remove #(= % :crossover-padding)
            (map #(if (< (rand) 0.5) %1 %2)
                 shorter-padded
                 longer))))

(defn diploid-crossover
  "Crosses over two individuals using uniform crossover with pairs of Push instructions.
   Pads shorter one from the end of the list of instructions."
  [plushy-a plushy-b]
  (let [plushy-a (partition 2 plushy-a)
        plushy-b (partition 2 plushy-b)
        shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    (flatten (remove #(= % :crossover-padding)
                     (map #(if (< (rand) 0.5) %1 %2)
                          shorter-padded
                          longer)))))

(defn tail-aligned-diploid-crossover
  "Crosses over two individuals using uniform crossover with pairs of Push instructions.
   Pads shorter one from the beginning of the list of instructions."
  [plushy-a plushy-b]
  (let [plushy-a (partition 2 plushy-a)
        plushy-b (partition 2 plushy-b)
        shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat (repeat length-diff :crossover-padding) shorter)]
    (flatten (remove #(= % :crossover-padding)
                     (map #(if (< (rand) 0.5) %1 %2)
                          shorter-padded
                          longer)))))

(defn rand-round 
  "Rounds x to an integer. If x has a fractional part, it's treated as a 
   probability of rounding x up."
  [x]
  (+ (int x) (if (< (rand) (- x (int x))) 1 0)))

(defn uniform-addition
  "Returns plushy with new instructions possibly added before or after each
  existing instruction."
  [plushy instructions umad-rate & {:keys [weights]}]
  (let [new-instr #(utils/random-instruction instructions :weights weights)]
    (mapcat #(shuffle (concat [%] (repeatedly (rand-round umad-rate) new-instr))) plushy)))

(defn uniform-replacement
  "Returns plushy with new instructions possibly replacing existing
   instructions."
  [plushy instructions replacement-rate]
  (map #(if (< (rand) replacement-rate)
          (utils/random-instruction instructions)
          %)
       plushy))

(defn diploid-uniform-silent-replacement
  "Returns plushy with new instructions possibly replacing existing
   instructions, but only among the silent member of each pair."
  [plushy instructions replacement-rate]
  (interleave (map first (partition 2 plushy))
              (map #(if (< (rand) replacement-rate)
                      (utils/random-instruction instructions)
                      %)
                   (map second (partition 2 plushy)))))

(defn diploid-uniform-addition
  "Returns plushy with new instructions possibly added before or after each
  existing instruction."
  [plushy instructions umad-rate]
  (flatten
    (map (fn [pair]
           (if (< (rand) umad-rate)
             (shuffle [pair (repeatedly 2 #(utils/random-instruction instructions))])
             [pair]))
         (partition 2 plushy))))

(defn uniform-deletion
  "Randomly deletes instructions from plushy at some rate."
  [plushy umad-rate]
  (if (zero? umad-rate)
    plushy
    (remove (fn [_] (< (rand)
                       (/ 1 (+ 1 (/ 1 umad-rate)))))
            plushy)))

(defn diploid-uniform-deletion
  "Randomly deletes instructions from plushy at some rate."
  [plushy umad-rate]
  (flatten (remove (fn [_] (< (rand)
                              (/ 1 (+ 1 (/ 1 umad-rate)))))
                   (partition 2 plushy))))

(defn diploid-flip
  "Randomly flips pairs in a diploid plushy at some rate."
  [plushy flip-rate]
  (flatten (map #(if (< (rand) flip-rate)
                   (reverse %)
                   %)
                (partition 2 plushy))))

(defn clamp
  [x & {:keys [low high]}]
  (let [x (if low (max low x) x)
        x (if high (min high x) x)]
    x))

(defn meta-mutate-gsemr 
  "Meta mutation rate from the group elites paper"
  [rate sigma]
  (* rate (math/pow sigma (- (* 2 (rand)) 1))))


(defn mutate-gsemr
  "UMAD mutation from the group elites paper"
  [individual argmap & {:keys [rate]}]
  (let [rate (or rate (:umad-rate individual))
        keep-rate (/ 1 (inc rate))
        {instructions :instructions} argmap
        {plushy :plushy} individual]
    ;; (println {:variation-equal (= new-plushy plushy)})
    {:plushy (if (< rate 5)
               (-> plushy
                   (uniform-addition instructions rate)
                   (uniform-deletion rate))
               (into [] (mapcat identity
                                (map (fn [instr]
                                       (shuffle 
                                        (concat 
                                         (when (< (rand) keep-rate) [instr])
                                         (repeatedly (dist/rand-binomial (rand-round rate) keep-rate) #(utils/random-instruction instructions)))))
                                     plushy))))
     :rate rate}))

(defn inherit 
  [individual parents argmap]
  (if (= (count parents) 1)
    (if-let [d (:inherit-depth argmap)]
      (assoc individual :inherited-errors (into [] (take d (:inherited-errors (first parents))))) individual)
    individual))

(def num-parents 
  {:crossover 2 
   :tail-aligned-crossover 2 
   :umad 1 
   :rumad 1 
   :uniform-addition 1 
   :uniform-replacement 1 
   :diploid-uniform-silent-replacement 1
   :uniform-deletion 1 
   :diploid-crossover 2 
   :tail-aligned-diploid-crossover 2 
   :diploid-umad 1 
   :diploid-uniform-addition 1 
   :diploid-uniform-deletion 1 
   :diploid-flip 1 
   :reproduction 1})

(def variation
  {:crossover (fn [parents _] (apply crossover (map :plushy parents)))
   :tail-aligned-crossover (fn [parents _] (apply tail-aligned-crossover (map :plushy parents)))
   :umad (fn [[{plushy :plushy}] argmap] (-> plushy
                                             (uniform-addition (:instructions argmap) (:umad-rate argmap))
                                             (uniform-deletion (:umad-rate argmap))))
   :rumad (fn [[{plushy :plushy}] argmap] (let [after-addition (uniform-addition plushy
                                                                                 (:instructions argmap)
                                                                                 (:umad-rate argmap))
                                                effective-addition-rate (/ (- (count after-addition)
                                                                              (count plushy))
                                                                           (count plushy))]
                                            (uniform-deletion after-addition effective-addition-rate)))
   :uniform-addition (fn [[{plushy :plushy}] argmap] (uniform-addition plushy (:instructions argmap) (:umad-rate argmap)))
   :uniform-replacement (fn [[{plushy :plushy}] argmap] (uniform-addition plushy (:instructions argmap) (:replacement-rate argmap)))
   :diploid-uniform-silent-replacement (fn [[{plushy :plushy}] argmap] (diploid-uniform-silent-replacement plushy (:instructions argmap) (:replacement-rate argmap)))
   :uniform-deletion (fn [[{plushy :plushy}] argmap] (uniform-deletion plushy (:umad-rate argmap)))
   :diploid-crossover (fn [parents _] (apply diploid-crossover (map :plushy parents)))
   :tail-aligned-diploid-crossover (fn [parents _] (apply diploid-crossover (map :plushy parents)))
   :diploid-umad (fn [[{plushy :plushy}] argmap] (-> plushy
                                             (diploid-uniform-addition (:instructions argmap) (:umad-rate argmap))
                                             (diploid-uniform-deletion (:umad-rate argmap))))
   :diploid-uniform-addition (fn [[{plushy :plushy}] argmap] (diploid-uniform-addition plushy (:instructions argmap) (:umad-rate argmap)))
   :diploid-uniform-deletion (fn [[{plushy :plushy}] argmap] (diploid-uniform-deletion plushy (:umad-rate argmap)))
   :diploid-flip (fn [[{plushy :plushy}] argmap] (diploid-flip plushy (:diploid-flip-rate argmap)))
   :reproduction (fn [[{plushy :plushy}] _] plushy)})


(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [pop argmap & {:keys [preselected?]}]
  (let [r (rand)
        op (loop [accum 0.0
                  ops-probs (vec (:variation argmap))]
             (if (empty? ops-probs)
               :reproduction
               (let [[op1 prob1] (first ops-probs)]
                 (if (>= (+ accum prob1) r)
                   op1
                   (recur (+ accum prob1)
                          (rest ops-probs))))))]
    (if-let [f (variation op)]
      (let [parents (if preselected? pop (repeatedly (num-parents op) #(selection/select-parent pop argmap)))]
        (inherit {:plushy (f parents argmap)}
                 parents argmap))
      (throw #?(:clj  (Exception. (str "No match in new-individual for " op))
                :cljs (js/Error
                       (str "No match in new-individual for " op)))))))
