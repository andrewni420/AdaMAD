(ns propeller.utils
  "Useful functions."
  (:require [clojure.zip :as zip]
            [clojure.repl :as repl]
            [propeller.tools.metrics :as metrics]
            [propeller.tools.math :as math]))

(defn first-non-nil
  "Returns the first non-nil values from the collection, or returns `nil` if
  the collection is empty or only contains `nil`."
  [coll]
  (first (filter some? coll)))

(defn indexof
  "Returns the first index of an element in a collection. If the element is not
  present in the collection, returns -1."
  [element coll]
  (or (first (keep-indexed #(when (= element %2) %1) coll)) -1))

(defn not-lazy
  "Returns lst if it is not a seq, or a non-lazy version of lst if it is."
  [lst]
  (if (seq? lst)
    (apply list lst)
    lst))

(defn ensure-list
  "Returns a non-lazy list if passed a seq argument. Otherwise, returns a list
  containing the argument."
  [thing]
  (if (seq? thing)
    (not-lazy thing)
    (list thing)))



(defn choice
  "Given items and weights, chooses a random item based on weights."
  ([items weights]
   (let [weights (or weights (repeat (count items) 1))
         weights (reductions + weights)
         total   (last weights)
         choices (map vector items weights)
         choice (* (rand) total)]
     (loop [[[c w] & more] choices]
       (when w
         (if (< choice w)
           c
           (recur more))))))
  ([items]
   (choice items nil)))


(defn choice-test 
  [] 
  (let [items [1 2 3 4 5 6]]
    (println (choice items))))

(defn random-instruction
  "Returns a random instruction from a supplied pool of instructions, evaluating
  ERC-producing functions to a constant literal."
  [instructions & {:keys [weights]}]
  (let [instruction (choice instructions weights)]
    (if (fn? instruction)
      (instruction)
      instruction)))


(defn count-points
  "Returns the number of points in tree, where each atom and each pair of parentheses
   counts as a point."
  [tree]
  (loop [remaining tree
         total 0]
    (cond (not (seq? remaining))
          (inc total)
          ;;
          (empty? remaining)
          (inc total)
          ;;
          (not (seq? (first remaining)))
          (recur (rest remaining)
                 (inc total))
          ;;
          :else
          (recur (concat (first remaining)
                         (rest remaining))
                 (inc total)))))

(defn seq-zip
  "Returns a zipper for nested sequences, given a root sequence"
  {:added "1.0"}
  [root]
  (zip/zipper seq?
              seq
              (fn [node children] (with-meta children (meta node)))
              root))

(defn depth
  "Returns the height of the nested list called tree.
  Borrowed idea from here: https://stackoverflow.com/a/36865180/2023312
  Works by looking at the path from each node in the tree to the root, and
  finding the longest one.
  Note: does not treat an empty list as having any height."
  [tree]
  (loop [zipper (seq-zip tree)
         height 0]
    (if (zip/end? zipper)
      height
      (recur (zip/next zipper)
             (-> zipper
                 zip/path
                 count
                 (max height))))))

(defn test-and-train-data-from-domains
  "Takes a list of domains and creates a set of (random) train inputs and a set of test
   inputs based on the domains. Returns [train test]. A program should not
   be considered a solution unless it is perfect on both the train and test
   cases."
  [domains]
  (vec
   (apply
    mapv
    concat
    (map (fn [[input-set n-train n-test]]
           (if (fn? input-set)
             (vector (repeatedly n-train input-set)
                     (repeatedly n-test input-set))
             (let [shuffled-inputs (shuffle input-set)
                   train-inputs (if (= n-train (count input-set))
                                  input-set ; NOTE: input-set is not shuffled if the same size as n-train
                                  (take n-train shuffled-inputs))
                   test-inputs (if (= n-test (count input-set))
                                 input-set ; NOTE: input-set is not shuffled if the same size as n-test
                                 (drop n-train shuffled-inputs))]
               (assert (= (+ n-train n-test) (count input-set))
                       "Sizes of train and test sets don't add up to the size of the input set.")
               (vector train-inputs test-inputs))))
         domains))))

(defn pmapallv
  "A utility for concurrent execution of a function. If :single-thread-mode is 
   truthy in the final arg then this acts like mapv of f on the provided colls. 
   Otherwise it acts like pmap but: 1) the colls should be finite, 2) the 
   returned sequence will not be lazy, and will in fact be a vector, and 
   3) calls to f may occur in any order, to maximize multicore processor utilization."
  [f & colls-args]
  #?(:clj (vec (if (:single-thread-mode (last colls-args))
                 (apply mapv f (butlast colls-args))
                 (let [agents (map #(agent % :error-handler
                                           (fn [agnt except]
                                             (repl/pst except 1000)
                                             (System/exit 0)))
                                   (apply map vector (butlast colls-args)))]
                   (dorun (map (fn [a] (send a #(apply f %))) agents))
                   (apply await agents)
                   (doall (mapv deref agents)))))
     :cljs (apply mapv f (butlast colls-args))))

(defn pmapallv2
  "A utility for concurrent execution of a function. If :single-thread-mode is 
   truthy in the final arg then this acts like mapv of f on the provided colls. 
   Otherwise it acts like pmap but: 1) the colls should be finite, 2) the 
   returned sequence will not be lazy, and will in fact be a vector, and 
   3) calls to f may occur in any order, to maximize multicore processor utilization."
  [f & colls-args]
  #?(:clj (vec (let [agents (map #(agent % :error-handler
                                         (fn [agnt except]
                                           (repl/pst except 1000)
                                           (System/exit 0)))
                                 (apply map vector colls-args))]
                 (dorun (map (fn [a] (send a #(apply f %))) agents))
                 (apply await agents)
                 (doall (mapv deref agents))))
     :cljs (apply mapv f colls-args)))


(defn drop-nth
  "drops the nth element from a collection"
  [n coll]
  ;(prn {:func :drop-nth :n n :coll coll})
  (concat
   (take n coll)
   (nthrest coll (inc n))))

(defn filter-by-index
  "filters a collection by a list of indices"
  [coll idxs]
  ;(prn {:func :filter-by-index :coll coll :idxs idxs})
  (map (partial nth coll) idxs))
