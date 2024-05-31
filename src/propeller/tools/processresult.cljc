(ns propeller.tools.processresult
  (:require [propeller.tools.math :as math]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
   #?(:cljs [cljs.reader :refer [read-string]]))
  (:import java.util.regex.Pattern))

(defn w-commas 
  [coll]
  (if (coll? coll)
    (apply str 
         (concat ["["] 
                 (drop-last (interleave (map w-commas coll) (repeat ", "))) 
                 ["]"]))
    coll))

(defn success-generation
  [filename & {:keys [generalize]}]
  (println filename)
  (let [file (slurp filename)
        file [(read-string (last (string/split file #"\n")))]
        ;; file (read-string (str "[" file "]"))
        ;; file (string/join "\n" (drop 1 (string/split file #"\n")))
        ;; file (string/replace file #":mapper .+?\],?" "")
        ;; file (take-last 3 (read-string (str "[" file "]")))
        _ (println file)
        x (if generalize 
            (first (filter identity (map :success-generation (filter #(and (not (nil? %)) (> 0.001 (:total-test-error %))) file))))
            (first (filter identity (map :success-generation (filter #(and (not (nil? %)) ((fn [n] (not (nil? n))) (:total-test-error %))) file))))) 
        ]
  (println {:x x})
    x
    #_(when (or (not generalize) (let [[t] (filter identity (map :total-test-error file))]
                                 (and (not (nil? t)) (zero? t))))
      (first (map :success-generation (filter #(= (:total-test-error %) 0) file))))))

(defn grab-generations
  [directory num-trials & {:keys [max-generation job]
                           :as argmap}]
  (let [max-generation (or (read-string (get argmap ":max-generation" "nil")) 300)
        job (or (read-string (get argmap ":job" "nil")) "")
        num-trials (read-string num-trials)
        files (map (partial str "/home/ani24/propeller-master/" directory "/")
                   (for [i (range num-trials)]
                     (str job (inc i) ".out")))
        generations (mapv success-generation files)]
    (println (mapv #(or % max-generation) generations))))

(defn is-bandit? 
  [line]
  (or (string/starts-with? line "Best error")
      (string/starts-with? line "errors")
      (string/starts-with? line "generation")
      (string/starts-with? line "bandits")))

;; (is-bandit? "generation  9896")

(defn grab-bandit-errors
  [filename & {:keys [max-generation]}]
  (let [errors (->> (string/split (slurp filename) #"\n")
                    (drop-while #(not (string/starts-with? % "Best error")))
                    (take-while is-bandit?)
                    (take-nth 4)
                    (map #(read-string (re-find #"\d+\.*\d*" %))))]
    (if max-generation
      (concat errors (repeat (- (inc max-generation) (count errors)) (last errors)))
      errors)))

(defn grab-all-generations
  [file]
  (let [grab-gen (fn [file] )]
    ))

(defn grab-normal-errors
  [filename & {:keys [max-generation]}]
  (when-let [file (try (let [w (slurp filename)] (read-string (str "[" (string/join (drop (.indexOf w "{:generation 0") w)) "]")))
                     (catch Exception e nil))]
    (let [errors (filter identity
                         (map :best-total-error
                              file))]
      (if max-generation
        (concat errors (repeat (- (inc max-generation) (count errors)) (last errors)))
        errors))))



(defn grab-and-spit-errors
  [directory num-files & {:keys [out bandit? max-generation]
                          :or {out "errors.out"} 
                          :as argmap}]
  (let [num-files (if (string? num-files) (read-string num-files) num-files)
        bandit? (read-string (get argmap ":bandit?" "nil"))
        max-generation (or (read-string (get argmap ":max-generation" "nil")) max-generation)
        out (or (read-string (get argmap ":out" "nil")) out)
        grabber (if bandit?
                  #(grab-bandit-errors % :max-generation max-generation)
                  #(grab-normal-errors % :max-generation max-generation))
        files (map (partial str "/home/ani24/propeller-master/" directory "/")
                   (for [i (range num-files)]
                     (str (inc i) ".out")))
        errors (filter identity (map grabber files))]
    (assert (every? #(= (count %) (inc max-generation)) errors), 
            (str "Errors: " (into [] (map count errors)) "\nMax generation: " max-generation))
    (spit (str "/home/ani24/propeller-master/" directory "/" out)
          (w-commas errors))))

(defn median-error
  [directory num-files & {:keys [num-cases avoiding]
                          :as argmap}]
  (let [num-files (read-string num-files)
        num-cases (or (read-string (get argmap ":num-cases" "nil")) 1)
        avoiding (or (set (read-string (get argmap ":avoiding" "nil"))) [])
        files (map (partial str "/home/ani24/propeller-master/" directory "/")
                   (for [i (range num-files) :when (not (contains? avoiding i))]
                     (str (inc i) ".out")))
        grab-error (fn [file] (try (:best-total-error (let [w (slurp file)] (read-string (string/join (drop (.indexOf w "{:generation 300") w)))))
                                   (catch Exception e nil)))
        errors (map #(/ % num-cases) (filter identity (map grab-error files)))]
    (println {:median (math/median errors)
              :mean (math/mean errors)
              :errors errors})))

(defn grab-genome
  [filename]
  (let [file (slurp filename)
        file (string/join "\n" (rest (string/split file #"\n")))
        file (string/replace file #":mapper .+?\],?" "")
        file (take-last 4 (read-string (str "[" file "]")))]
    (when (let [[t] (filter identity (map :total-test-error file))]
            (and (not (nil? t)) (zero? t)))
      {:unsimplified (last (filter identity (map :best-program file)))
       :simplified (last (filter identity (map :simplified-program file)))})))

(defn grab-genomes
  [directory num-files & {:keys [unsimplified?] :as argmap}]
  (let [num-files (if (number? num-files) num-files (read-string num-files))
        files (map (partial str "/home/ani24/propeller-master/" directory "/")
                   (for [i (range num-files)] (str (inc i) ".out")))
        genomes (mapv grab-genome files)
        unsimplified? (or unsimplified? (read-string (get argmap ":unsimplified?" "nil")))]
    (if unsimplified? 
      (pprint/pprint genomes) 
      (pprint/pprint (mapv #(dissoc % :unsimplified) genomes)))))

(defn -main
  [num-trials directory & {:keys [max-generation job spit-errors]
                           :as argmap}]
  (let [max-generation (or (read-string (get argmap ":max-generation" "nil")) 300)
        job (or (read-string (get argmap ":job" "nil")) "")
        spit-errors (read-string (get argmap ":spit-errors" "nil"))
        num-trials (read-string num-trials)
        files (map (partial str "/home/ani24/propeller-master/" directory "/")
                   (for [i (filter #(not (contains? #{1} %)) (range num-trials))]
                     (str job (inc i) ".out")))
        successes (mapv #(success-generation % :generalize false) files)
        generalizing (mapv #(success-generation % :generalize true) files)
        ;; successes generalizing
        _ (println generalizing)
        report (fn [generations]
                 {:average-success-generation (math/mean (filter identity generations))
                  :average-overall-generation (math/mean (map #(or % max-generation) generations))
                  :success-count (count (filter identity generations))
                  :successes (filter identity (map-indexed #(if %2 (inc %1) nil) generations))})]
    (println {:successes (report successes)})
    (println {:generalizing (report generalizing)})
    (spit (str "/home/ani24/propeller-master/" directory "/successes.out")
          (with-out-str (println {:successes (report successes)})
                        (println {:generalizing (report generalizing)})))
    ;; (spit (str "/home/ani24/propeller-master/" directory "/genomes.out")
    ;;       (with-out-str (grab-genomes directory num-trials :unsimplified? true)))
    (when spit-errors (grab-and-spit-errors directory num-trials :max-generation max-generation))))

;;ls | grep -P "^slurm" | xargs -d "\n" rm
