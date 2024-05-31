(ns propeller.tools.maxbandit
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
            [propeller.tools.distributions :as dist]
            [propeller.utils :as utils]
            [cheshire.core :as cheshire]
            [propeller.tools.downsample :as downsample])
  (:import java.lang.Double
           java.lang.Class))

(set! *warn-on-reflection* true)

(defn mean
  [coll]
  (let [coll (filter identity coll)
        s (reduce + coll)
        n (count coll)]
    (if (= 0 n)
      0
      (/ s n))))

(defn amean
  "Mean of a primitive double array"
  [^doubles coll]
  (let [n (alength coll)]
    (if (zero? n) 0.0
        (/ (areduce coll c-idx c-ret 0. (+ c-ret (aget coll c-idx)))
           (alength coll)))))

(defn make-coll
  "Turn x into a collection by repeating n times, unless it's already a collection, in which case return
   it unchanged. The second parameter can either be a number or a collection, in which case x will match 
   its length"
  [x n]
  (if (coll? x)
    x
    (repeat x (if (coll? n)
                (count n)
                n))))

(defn epanechikov-single
  "Epanechikov kernel function for 1-d data"
  ([u]
   (if (< (math/abs u) 1)
     (* (/ 3 4) (- 1 (* u u)))
     0))
  ([u h]
   (epanechikov-single (/ u h)))
  ([x1 x2 h]
   (epanechikov-single (/ (- x1 x2) h))))

(defn epanechikov
  "Epanechikov kernel. Supports 1d and multiple dimensional datapoints"
  ([u]
   (if (coll? u)
     (transduce (map epanechikov-single) * 1 u)
     (epanechikov-single u)))
  ([u h]
   (if (coll? u)
     (reduce * 1 (map epanechikov-single u (make-coll h (count u))))
     (epanechikov-single u h)))
  ([x1 x2 h]
   (if (some coll? [x1 x2])
     (let [n (count (first (filter coll? [x1 x2])))]
       (reduce * 1 (map epanechikov-single
                        (make-coll x1 n)
                        (make-coll x2 n)
                        (make-coll h n))))
     (epanechikov-single x1 x2 h))))


(defn lambert-w
  "Computes the Lambert w function for the principal branch. 
   Use an approximation for large numbers, and a cubically convergent iteration for small numbers"
  ([z]
   (let [z1 (math/log z)
         z2 (math/log z1)]
     (+ z1 (- z2) (/ z2 z1))))
  ([z res & {:keys [max-iter] :or {max-iter 10}}]
   (loop [guess 0.
          error ##Inf
          n-iter 0]
     (cond (< error res) guess
           (> n-iter max-iter) guess
           :else (let [^double e-guess (math/exp guess)
                       frac1 (/ (* (+ 2 guess) (- (* guess e-guess) z))
                                (+ 2 (* 2 guess)))
                       diff (/ (- (* guess e-guess) z)
                               (- (* e-guess (+ 1 guess)) frac1))
                       error (double (math/abs diff))]
                   (recur (- guess diff)
                          error
                          (inc n-iter)))))))

(defn _nth
  [coll i]
  (if (>= i 0)
    (nth coll i)
    (nth coll (+ (count coll) i))))

(defn rand-normal
  "Marsaglia polar method for a pair of independent normal random variables\\
   If pair? is false, throws away the second generated number.\\
   -> [float1 float2]"
  [m std & {:keys [pair?]}]
  (loop [u (-> (rand) (* 2) (- 1))
         v (-> (rand) (* 2) (- 1))]
    (let [s (+ (math/square u) (math/square v))
          s1 (Math/sqrt
              (/
               (* (- 2) (Math/log s))
               s))]
      (if (and (< s 1) (> s 0))
        (if pair?
          [(+ m (* std u s1))
           (+ m (* std v s1))]
          (+ m (* std u s1)))
        (recur (rand) (rand))))))


(defn normalize
  [v]
  (let [total (math/sqrt (transduce (map #(* % %)) + v))]
    (mapv #(/ % total) v)))

(defn gram-schmidt
  [& vectors]
  (loop [vecs vectors
         i 0]
    (if (= i (dec (count vectors)))
      (mapv normalize vecs)
      (let [total (transduce (map math/square) + (nth vecs i))
            coefs (map-indexed #(if (<= %1 i) 0 (/ (math/dot %2 (nth vecs i)) total)) vecs)
            normalized (mapv (fn [c v] (mapv #(- %1 (* %2 c)) v (nth vecs i))) coefs vecs)]
        #_(pprint/pprint {:i i :vecs vecs :normalized normalized :coefs coefs :other-coefs (mapv #(math/dot % (nth vecs i)) vecs)})
        (recur normalized
               (inc i))))))



(defn make-random-basis
  "Make a random orthonormal basis set of vectors."
  [size num-cases & {:keys [scale] :or {scale 1}}]
  (mapv double-array
        (apply gram-schmidt (repeatedly size #(repeatedly num-cases (partial rand-normal 0 1))))))


(def E "1E-10" 1e-10)

#_(defmacro cartesian-product
    [sequences]
    (let [symbs (repeatedly (count sequences) gensym)]
      `(into []
             (for [~@(mapcat vector symbs sequences)]
               [~@symbs]))))

(defn cartesian-product
  "Cartesian product of collections. Returns a list of [x1 x2 x3 ...] points"
  [colls]
  (if (empty? colls)
    '[[]]
    (for [more (cartesian-product (drop-last colls))
          x (last colls)]
      (conj more x))))

(defmacro make-fn
  "Turns a macro into a function"
  [m]
  `(fn [& args#]
     (eval
      (cons '~m args#))))


(defn last-block-max
  "Take the max of the last n items in the collection when the length of the collection
   is a multiple of n. Otherwise returns default"
  [coll n default]
  (if (zero? (mod (count coll) n))
    (apply max (take-last n coll))
    default))

(defn estimate-mi
  "Given samples, uses the median of means method to estimate 
   the expectation of the maximum over i draws."
  [samples i]
  (let [block-max (map (partial apply max) (partition i samples))]
    (mean block-max)))

(defn initialize-mi
  "Initialize the mi max values by sampling from a very small normal distribution. 
   Prevents edge cases where all mi values are equal, resulting in arithmetic errors"
  []
  (let [elements (repeatedly 4 #(math/abs (rand-normal 0 E)))]
    (mapv (partial estimate-mi elements) [1 2 4])))

(defn log2
  "Log base 2"
  [x]
  (/ (Math/log x) (Math/log 2)))

(defn estimate-xi
  "O(ln(1/δ)1/ε^2) draws needed where ε=ξ*/3 and δ=δ/2"
  [m1 m2 m4]
  (when (zero? (- m2 m1)) (println "zero denominator" m1 m2 m4))
  (log2 (/ (- m4 m2) (- m2 m1))))

(defn interpolate-m
  "If xi~0, mn = m1 + (m2-m1)log2(n)\\
   Otherwise:\\
   a1 = (m1m4-m2^2)/(m1-2m2+m4)\\
   a2 = (-2m1m2 + m1^2 + m2^2)/(m1-2m2+m4)\\
   a3 = (m4-m2)/(m2-m1)\\
   mn = a1 + a2*a3^log2(n)"
  [xi xi* m1 m2 m4 n]
  (if (< (math/abs xi) xi*)
    (+ m1 (* (- m2 m1) (log2 n)))
    (let [temp (+ (- m1 (* 2 m2)) m4)
          a1 (/ (- (* m1 m4) (math/square m2))
                temp)
          a2 (/ (+ (- (* 2 m1 m2))
                   (math/square m1)
                   (math/square m2))
                temp)
          a3 (/ (- m4 m2) (- m2 m1))]
      (+ a1 (* a2 (Math/pow a3 (log2 n)))))))



(defn estimate-m-sample
  "Given samples, estimates mn, the expectation of the maximum over n draws."
  [samples n xi*]
  (if (> 4 (count samples))
    ##Inf
    (let [[m1 m2 m4] (map #(estimate-mi samples %) [1 2 4])
          xi (estimate-xi m1 m2 m4)]
      (interpolate-m xi xi* m1 m2 m4 n))))

(defn estimate-m-mi
  "Estimates the max over n draws using the maxes over 1, 2, and 4 draws"
  [mi n xi*]
  (let [[m1 m2 m4] mi
        xi (estimate-xi m1 m2 m4)]
    (interpolate-m xi xi* m1 m2 m4 n)))


(defn update-mi
  "Updates the mi using a learning rate"
  [m n coll lr]
  (let [lr (Math/pow lr n)]
    (+ (* lr (last-block-max coll n m)) (* (- 1 lr) m))))

(defn update-mi-chunked
  "Updates the mis in a chunked way, where if the collection has 4 elements, m4 is updated using the max,
   m2 using the average max of two elements, etc. Allows for momentum with dampening."
  [m n coll lr & {:keys [momentum momentum-lr dampening]}]
  ;; (pprint/pprint {:m m :n n :coll coll :lr lr :momentum momentum :momentum-lr momentum-lr :dampening dampening})
  (if (>= (count coll) 4)
    (let [dampening (if dampening dampening momentum-lr)
          target (mean (map (partial apply max) (partition n coll)))]
      (if momentum
        (let [grad (* 2 lr (- m target))
              momentum (if momentum (+ (* (- 1 momentum-lr) momentum) (* dampening grad)) nil)
              grad (+ grad (if momentum (* (- 1 momentum-lr) momentum) 0))]
          [(- m grad) momentum])
        (+ (* lr target)
           (* (- 1 lr) m))))
    m))


(defn single-tile
  "Construct a single dimension of the tile coding"
  [l to ta r special]
  (let [intervals (range (+ l to) r ta)]
    (into []
          (concat (map (partial into [])
                       (partition 2 1 (concat (if (zero? to) [] [l])
                                              (if (< (math/abs (- (last intervals) r)) E) (drop-last intervals) intervals)
                                              [r])))
                  (sort-by special (map first special))))))

(defn get-tiles
  "Construct the tiles in a tile coding"
  [& tiles]
  (let [encodings (map (partial apply single-tile) tiles)]
    (cartesian-product encodings)))


(defn e-greedy
  "Epsilon greedy algorithm"
  [epsilon rewards]
  (if (< (rand) epsilon)
    (rand-int (count rewards))
    (apply max-key #(mean (rewards %)) (range (count rewards)))))

#_(frequencies
   (repeatedly 10000
               #(e-greedy 0.5
                          [[0] [1] [2] [3] [4] [5] [4] [2] [1] [0]])))

(defn sample-uniform
  "Sample uniformly from the interval from left to right"
  [left right & {:keys [low high sigma]}]
  (let [low (or low left)
        high (or high right)
        sigma (or sigma 0)
        unif (+ left (* (rand) (- right left)))
        mutated (+ unif (rand-normal 0 sigma))]
    (min high (max low mutated))))

(defn depth
  "Returns the depth of the collection"
  [coll & {:keys [start]
           :or {start 0}}]
  (if (coll? coll)
    (recur (first coll) {:start (inc start)})
    start))

(defn deep-array
  "Turns the collection into a nested primitive array"
  [coll & {:keys [type]
           :or {type ["D" Double/TYPE double]}}]
  (let [d (depth coll)]
    (condp = d
      0 ((nth type 2) coll)
      1 (into-array (second type) coll)
      (into-array (Class/forName (apply str
                                        (concat (repeat (dec d) "[")
                                                [(first type)])))
                  (map deep-array coll)))))


(defn to-single-tile
  "Identifies the index of the tile containing the given point"
  [x l r to ta & {:keys [special]}]
  (if (number? x)
    (int ((if (zero? to)
            #(Math/floor %)
            #(Math/ceil %))
          (/
           (min (max (- x to l) 0) (- r l))
           ta)))
    (special x)))

(defn expand-coding-kde
  [coding]
  (let [{l :l
         r :r
         to :to
         ta :ta
         dist-acc :dist-acc} coding
        tiles (cartesian-product (mapv #(range (+ %1 %2) %3 %4) l to r ta))
        n-tiles (math/round (/ 2 dist-acc))
        dist-pos (map #(- (* % dist-acc) 1) (range n-tiles))]
    (merge coding
           {:tiles tiles
            ;; :tile-indices (into {} (map-indexed #(vector %1 %2) tiles))
            :dist (make-array Double/TYPE (count tiles) (count dist-pos))
            :dist-pos (double-array dist-pos)
            :N (make-array Double/TYPE (count tiles))
            :w (make-array Double/TYPE (count tiles))
            :evaluated #{}})))


(defn expand-test
  []
  (expand-coding-kde {:l [0] :r [1] :to [0.1] :ta [0.3]}))

(defn expand-coding
  [coding & {:keys [max-bandit AO special n xi* momentum? max-n kde lr]}]
  (let [{l :l
         r :r
         to :to
         ta :ta} coding
        tiles (apply get-tiles (mapv vector l to ta r special))
        num-tiles (mapv #(int (+ 1 (to-single-tile (- %4 E) %1 %4 %2 %3)
                                 (count %5))) l to ta r special)]
    (if kde
      (expand-coding-kde coding)
      (merge coding
             {:tiles tiles
              :num-tiles num-tiles
              :special special
              :w (into [] (repeat (count tiles) (if max-bandit [] 0)))
              :N (long-array (repeat (count tiles) 0))}
             (when (and AO (not max-n)) (let [mi (into-array (Class/forName "[D")
                                                             (repeatedly (count tiles)
                                                                         #(double-array (initialize-mi))))]
                                          {:mi mi
                                           :m (double-array (mapv #(estimate-m-mi % n xi*) mi))}))
             (when (and AO momentum? (not max-n))
               {:momentum (into-array (Class/forName "[D")
                                      (repeatedly (count tiles)
                                                  #(double-array [0 0 0])))})
             (when max-n
               {:max-n max-n
                :w (into-array (Class/forName "[D")
                               (repeatedly (count tiles)
                                           #(double-array (repeat (if (number? max-n) max-n (apply max max-n)) ##-Inf))))
                :N (long-array (repeat (count tiles) 0))
                :n-since-update (long-array (repeat (count tiles) 0))
                :m (double-array (repeat (count tiles) 0))})
             (when (and max-n momentum?)
               {:momentum (double-array (repeat (count tiles) 0))})
             (merge
              (when (contains? #{:adam :momentum} (:method lr)) {:momentum (double-array (repeat (count tiles) 0))})
              (when (contains? #{:adam :rmsprop} (:method lr)) {:gradvar (double-array (repeat (count tiles) 0))}))))))

;bandit = mode, l, r, to, ta, d, lr, acc, w, N, 
(defn tile-coding
  [l r to ta & {:keys [max-bandit AO special n xi* momentum? max-n dist-acc kde lr]}]
  (let [special (or special (mapv (constantly {}) l))]
    (expand-coding
     {:l l
      :r r
      :to (mapv rand-nth to)
      :ta (mapv rand-nth ta)
      :max-n max-n
      :dist-acc dist-acc}
     :max-bandit max-bandit
     :AO AO
     :n n
     :xi* xi*
     :kde kde
     :special special
     :momentum? momentum?
     :max-n max-n
     :lr lr)))


#_(tile-coding [0] [1] [[0]] [[0.4]] :max-bandit true :AO true :special [{:restart -1}])

;; (tile-coding [0] [1] [[0.1 0.2]] [[0.2]])

(defn mm-mul
  "Multiplicative moving maximum"
  [cur prev alpha]
  (max cur (* prev alpha)))

(defn mm-add
  "Additive moving maximum"
  [cur prev beta]
  (max cur (- prev beta)))

(defn ema
  "Exponential moving average"
  [cur prev alpha]
  (+ cur (* alpha prev)))

(defn movmax
  "Moving maximum"
  [cur prev param method]
  (assert (contains? ["add" "mul"] method))
  (if (= method "add")
    (mm-add cur prev param)
    (mm-mul cur prev param)))


(defn to-tile-index
  "Turns a continuous point into a discrete point in the space of the coding"
  [x coding]
  (let [{l :l
         r :r
         to :to
         ta :ta
         num-tiles :num-tiles
         special :special} coding
        indices (mapv to-single-tile x l r to ta (repeat :special) special)
        tile-count (reverse (drop-last (reductions * 1 (reverse num-tiles))))
        indices (mapv #(* %3 (if (> 0 %1) (+ %2 %1) %1)) indices num-tiles tile-count)]
    (reduce + indices)))

#_(defn to-tile-index
    "Turns a continuous point into a discrete point in the space of the coding"
    [x coding]
    (let [{l :l
           r :r
           to :to
           ta :ta
           num-tiles :num-tiles
           special :special} coding
          indices (mapv to-single-tile x l r to ta (repeat :special) special)
          indices (mapv #(if (> 0 %1) (+ %2 %1) %1) indices num-tiles)
          tile-count (reverse (drop-last (reductions * 1 (reverse num-tiles))))]
      (reduce + (map * indices tile-count))))


(defn print-return
  "Print argument and return it"
  [x]
  (println x)
  x)


#_(defn update-mi-chunked
    "Updates the mis in a chunked way, where if the collection has 4 elements, m4 is updated using the max,
   m2 using the average max of two elements, etc. Allows for momentum with dampening."
    [m n coll lr & {:keys [momentum momentum-lr dampening]}]
  ;; (pprint/pprint {:m m :n n :coll coll :lr lr :momentum momentum :momentum-lr momentum-lr :dampening dampening})
    (if (>= (count coll) 4)
      (let [dampening (if dampening dampening momentum-lr)
            target (mean (map (partial apply max) (partition n coll)))]
        (if momentum
          (let [grad (* 2 lr (- m target))
                momentum (if momentum (+ (* (- 1 momentum-lr) momentum) (* dampening grad)) nil)
                grad (+ grad (if momentum (* (- 1 momentum-lr) momentum) 0))]
            [(- m grad) momentum])
          (+ (* lr target)
             (* (- 1 lr) m))))
      m))

(defn sgd-vanilla
  "Implements the vanilla SGD algorithm"
  [output target lr]
  (let [grad (* 2 lr (- output target))]
    {:output (- output grad)}))

(defn sgd-with-momentum
  "Implements the SGD with momentum learning algorithm"
  [output target lr momentum & {:keys [momentum-lr dampening t warm-start] :or {momentum-lr 0.1}}]
  (let [grad (* 2 lr (- output target))
        dampening (or dampening momentum-lr)
        momentum (if momentum-lr (+ (* (- 1 momentum-lr) momentum) (* dampening grad)) nil)
        mhat (if (and momentum-lr warm-start t (< t warm-start))
               (/ momentum (- 1 (math/pow (- 1 momentum-lr) (inc t))))
               momentum)
        grad (+ grad (if momentum-lr (* (- 1 momentum-lr) mhat) 0))]
    {:output (- output grad)
     :momentum momentum}))

(defn rmsprop
  "Implements the rmsprop learning algorithm"
  [output target lr gradvar & {:keys [epsilon gradvar-lr] :or {epsilon 1E-8 gradvar-lr 0.001}}]
  (let [epsilon (or epsilon 1E-8)
        grad (* 2 (- output target))
        gradvar (+ (* (- 1 gradvar-lr) gradvar)
                   (* gradvar-lr grad grad))
        grad (* grad (/ lr (math/sqrt (+ epsilon gradvar))))]
    {:output (- output grad)
     :gradvar gradvar}))

(defn adam
  "Implements the adam learning algorithm"
  [output target momentum gradvar & {:keys [epsilon alpha beta1 beta2 t warm-start]
                                     :or {epsilon 1E-8 alpha 0.0001 beta1 0.1 beta2 0.001}}]
  (let [epsilon (or epsilon 1E-8)
        grad (* 2 (- output target))
        momentum (+ (* (- 1 beta1) momentum)
                    (* beta1 grad))
        gradvar (+ (* (- 1 beta2) gradvar)
                   (* beta2 grad grad))
        mhat (if (and warm-start t (< t warm-start))
               (/ momentum (- 1 (math/pow (- 1 beta1) (inc t))))
               momentum)
        vhat (if (and warm-start t (< t warm-start))
               (/ gradvar (- 1 (math/pow (- 1 beta2) (inc t))))
               gradvar)
        grad (/ (* alpha mhat)
                (math/sqrt (+ epsilon vhat)))]
    {:output (- output grad)
     :momentum momentum
     :gradvar gradvar}))


(defn sgd
  [output target kwargs]
  (condp = (:method kwargs)
    :sgd (sgd-vanilla output target (:lr kwargs))
    :momentum (apply sgd-with-momentum output target (:lr kwargs) (:momentum kwargs) (into [] (mapcat identity kwargs)))
    :rmsprop (apply rmsprop output target (:lr kwargs) (:gradvar kwargs) (into [] (mapcat identity kwargs)))
    :adam (apply adam output target (:momentum kwargs) (:gradvar kwargs) (into [] (mapcat identity kwargs)))
    (throw (Exception. (str "Method " (:method kwargs) " not implemented")))))

(defn update-coding-maxn-legacy
  "Update tile coding when tracking the max of n samples. 
   Inserts reward into the list of rewards, takes the max, and updates the running estimate using SGD"
  [x r coding lr & {:keys [momentum-lr dampening]}]
  (let [{^longs N :N
         ^"[[D" w :w
         ^doubles m :m
         max-n :max-n
         ^doubles momentum :momentum} coding
        i (to-tile-index x coding)
        ^doubles wi (aget w i)
        dampening (or dampening momentum-lr)]
    (aset-double wi (mod (aget N i) max-n) r)
    (aset-long N i (inc (aget N i)))
    (let [target (apply max wi)
          current (aget m i)
          grad (* 2 lr (- current target))
          moment (if momentum-lr (+ (* (- 1 momentum-lr) (aget momentum i)) (* dampening grad)) nil)
          grad (+ grad (if momentum-lr (* (- 1 moment) moment) 0))
          updated-m (- current grad)]
      (aset-double m i updated-m)
      (when momentum (aset-double momentum i moment)))
    coding))


(defn increase-maxn
  "Increase the number of samples to max over"
  [coding lr]
  (let [{t :t} lr
        {^"[[D" w :w
         max-n :max-n} coding
        max-n (if (number? max-n) max-n (nth max-n t))
        prev-max-n (alength ^doubles (aget w 0))]
    (if (= prev-max-n max-n)
      coding
      (let [^"[[D" updated-w (make-array Double/TYPE (alength w) max-n)]
        (amap w idx ret (let [^doubles ww (aget w idx)
                              ^doubles updated-ww (aget updated-w idx)]
                          (amap updated-ww iidx rret (aset updated-ww iidx (if (< iidx prev-max-n) (aget ww iidx) ##-Inf)))))
        (assoc coding
               :w updated-w)))))

(defn nth-max
  [^doubles arr n max-n]
  (let [arr-n (alength arr)]
    (loop [i 0
           m ##-Inf]
      (if (= i max-n) m (recur (inc i) (max m (aget arr (mod (- n i) arr-n))))))))

(defn update-multi-maxn
  [^"[[D" m ^"[[D" momentum ^"[[D" gradvar idx cur-n ^doubles wi max-n]
  (let [targets (mapv #(nth-max wi cur-n %) max-n)
        output (mapv #(aget ^doubles (aget m %) idx) (range (count max-n)))]
    ()))

;Each individual gets associated with the tile it was sampled from
;after evaluation, update the tile associated with the individual
(defn update-coding-multi-maxn
  "Update tile coding when tracking the max of n samples. 
   Inserts reward into the list of rewards, takes the max, and updates the running estimate using SGD"
  [x r coding lr & {:keys [legacy]
                    :as kwargs}]
  (let [{update-freq :update-freq t :t decay :decay} lr
        update-freq (or update-freq 1)
        update-freq (if (number? update-freq) update-freq (nth update-freq t))
        decay (or decay 1)
        {^longs N :N
         ^longs n-since-update :n-since-update
         ^"[[D" w :w
         ^"[[D" m :m
         max-n :max-n
         ^"[[D" momentum :momentum
         ^"[[D" gradvar :gradvar
         :as coding} coding
        i (to-tile-index x coding)
        ^doubles wi (aget w i)]
    (aset-double wi (mod (aget N i) (alength wi)) r)
    (aset-long N i (inc (aget N i)))
    (aset-long n-since-update i (inc (aget n-since-update i)))
    (when (> (aget n-since-update i) update-freq)
      (let [{method :method} lr
            kwargs (merge lr
                          (when (contains? #{:adam :momentum} method) {:momentum (aget momentum i)})
                          (when (contains? #{:adam :rmsprop} method) {:gradvar (aget gradvar i)}))
            target (nth-max wi (dec (aget N i)) max-n)
                ;; _ (when (= target ##-Inf) (pprint/pprint {:target target :wi wi :N (dec (aget N i)) :max-n max-n}))
            {output :output
             cur-m :momentum
             cur-v :gradvar} (sgd (aget m i) target kwargs)]
        (aset-double m i output)
        (aset-long n-since-update i 0)
        (when cur-m (aset-double momentum i cur-m))
        (when cur-v (aset-double gradvar i cur-v))))
    (when-not (= decay 1) (amap m idx ret (aset m idx (* decay (aget m idx)))))
    coding))


(defn update-coding-maxn
  "Update tile coding when tracking the max of n samples. 
   Inserts reward into the list of rewards, takes the max, and updates the running estimate using SGD"
  [x r coding lr & {:keys [legacy]
                    :as kwargs}]
  (if legacy
    (update-coding-maxn-legacy x r coding lr :momentum-lr (:momentum-lr kwargs) :dampening (:dampening kwargs))
    (let [{update-freq :update-freq t :t decay :decay} lr
          update-freq (or update-freq 1)
          update-freq (if (number? update-freq) update-freq (nth update-freq t))
          decay (or decay 1)
          {^longs N :N
           ^longs n-since-update :n-since-update
           ^"[[D" w :w
           ^doubles m :m
           max-n :max-n
           ^doubles momentum :momentum
           ^doubles gradvar :gradvar
           :as coding} coding
          i (to-tile-index x coding)
          ^doubles wi (aget w i)]
      (aset-double wi (mod (aget N i) (alength wi)) r)
      (aset-long N i (inc (aget N i)))
      (aset-long n-since-update i (inc (aget n-since-update i)))
      (when (> (aget n-since-update i) update-freq)
        (let [{method :method} lr
              kwargs (merge lr
                            (when (contains? #{:adam :momentum} method) {:momentum (aget momentum i)})
                            (when (contains? #{:adam :rmsprop} method) {:gradvar (aget gradvar i)}))
              target (nth-max wi (dec (aget N i)) max-n)
              ;; _ (when (= target ##-Inf) (pprint/pprint {:target target :wi wi :N (dec (aget N i)) :max-n max-n}))
              {output :output
               cur-m :momentum
               cur-v :gradvar} (sgd (aget m i) target kwargs)]
          (aset-double m i output)
          (aset-long n-since-update i 0)
          (when cur-m (aset-double momentum i cur-m))
          (when cur-v (aset-double gradvar i cur-v))))
      (when-not (= decay 1) (amap m idx ret (aset m idx (* decay (aget m idx)))))
      coding)))


;; (defn kde-indices 
;;   "|x-r|<bw implies r-bw<x<r+bw
;;    mapping from index to position: lambda i: -1 + dist-acc*i
;;    mapping from position to index: lambda pos: (pos+1)/dist-acc
;;    (r-bw+1)/dist-acc < x_pos < (r+bw+1)/dist-acc
;;    returns [low high]"
;;   [r dist-acc bw]
;;   (let [low (max 0 (int (/ (+ 1 (- r bw)) dist-acc)))
;;         high (min (int (/ 2 dist-acc)) (int (math/ceil (/ (+ 1 (+ r bw)) dist-acc))))]
;;     [low high]))

(defn update-kde
  "Update the running kernel density estimation using the position of the reward 
   and the learning rate. 
   lr is the tile's learning rate 
   (epanechikov tile r bw) is the value of the kernel at the tile"
  [^doubles dist r bw lr ^doubles dist-pos]
  (let [x (amap dist idx _
                (aset dist idx (+ (* (- 1 lr) (aget dist idx))
                                  (* lr (epanechikov (aget dist-pos idx) r bw)))))]
    x))

(defn update-coding-kde
  "Updates the running density estimate using the observed reward. "
  [x r coding lr & {:keys [bandwidths]}]
  (let [{tiles :tiles
         ^"[[D" dist :dist
         dist-pos :dist-pos
         ^doubles N :N} coding
        ndim (count (first tiles))
        bandwidths (or bandwidths (repeat (inc ndim) 1))
        dist-bw (last bandwidths)
        tile-bw (drop-last bandwidths)
        tile-lr (into []
                      (comp (map-indexed #(vector %1 (* lr (epanechikov x %2 tile-bw))))
                            (filter #(not (zero? (second %)))))
                      tiles)]
    (mapv (fn [[idx lr]] (update-kde (aget dist idx) r dist-bw lr dist-pos)) tile-lr)
    (mapv (fn [[idx lr]] (aset N idx (+ lr (aget N idx)))) tile-lr)
    (update coding :evaluated set/union (set (map first tile-lr)))))

(defn trans-test
  []
  (pprint/pprint
   (into []
         (comp (map inc)
               (map (partial * 3)))
         (range 10))))

;Each bandit has multiple tile codings that cover the space up
(defn update-coding-AO
  [x r coding lr n xi* & {:keys [momentum-lr dampening]}]
  (let [{w :w
         ^longs N :N
         ^"[[D" mi :mi
         ^doubles m :m
         ^"[[D" momentum :momentum} coding
        i (to-tile-index x coding)
        ^doubles mii (aget mi i)
        ^doubles momentum-i (when momentum (aget momentum i))
        new-w (conj (nth w i) r)
        new-ww (if (>= (count new-w) 4) [] new-w)]
    (aset-long N i (inc (aget N i)))
    (when (>= (count new-w) 4)
      (areduce mii m-idx _ nil
               (let [mi-updated (update-mi-chunked (aget mii m-idx)
                                                   (int (Math/pow 2 m-idx))
                                                   new-w
                                                   lr
                                                   :momentum (when momentum-i (aget momentum-i m-idx))
                                                   :momentum-lr momentum-lr
                                                   :dampening dampening)]
                 (if momentum-i
                   (do
                     (aset-double mii m-idx (first mi-updated))
                     (aset-double momentum-i m-idx (second mi-updated)))
                   (aset-double mii m-idx mi-updated))))
      (aset-double m i (estimate-m-mi mii n xi*)))
    (merge coding {:w (assoc w i new-ww)})))


;; (defn -get
;;   [^"[[D" a]
;;   (aget a 2))

;; (defn -get2
;;   [a]
;;   (nth (nth a 2) 2))

;; (defn -get3
;;   [a]
;;   (aget a 2))

;; (let [arr (make-array Double/TYPE 3 3)]
;;   (time (dotimes [_ 10000]
;;           (-get arr))))

;; (let [d (double-array [0 0 0])]
;;   (amap d my-idx my-ret (do (aset d my-idx 10.)
;;                                                         (println (into [] my-ret))
;;                                                         (+ 1.0 my-idx))))






;I should keep running m1 m2 and m4 and then use them to estimate stuff
;Running m1 m2 m4 per tile and residual up to 3 element list

(defn update-coding
  "Uses learning rate to update the max-reward and num-visitation
   of each tile in the coding \\
   Maybe should use the GDI formuation w+=lr(g-V) instead?
   lr is close to 0 in the GDI formulation, but close to 1 in the max-bandit formulation"
  [x r coding lr & {:keys [max-bandit AO V t n xi* dampening momentum-lr kde bandwidths legacy]}]
  (let [{w :w
         N :N
         max-n :max-n} coding
        i (to-tile-index x coding)]
    (cond kde (update-coding-kde x r coding lr :bandwidths bandwidths :legacy legacy)
          max-n (update-coding-maxn x r coding lr :momentum-lr momentum-lr :dampening dampening :legacy legacy)
          AO (update-coding-AO x r coding lr n xi* :dampening dampening :momentum-lr momentum-lr)
          :else (if max-bandit
                  (assoc coding
                         :N (into []
                                  (map-indexed #(if (= i %1)
                                                  (ema 1 %2 lr)
                                                  (ema 0 %2 lr))
                                               N))
                         :w (into []
                                  (take-last n
                                             (map-indexed (fn [id e]
                                                            (filterv (partial < t)
                                                                     (conj (mapv #(- % (math/abs (* % (- 1 lr)))) e)
                                                                           (if (= i id)
                                                                             r
                                                                             ##-Inf))))
                                                          w))))
                  (assoc coding
                         :w (update w i (partial + (* lr (- r V))))
                         :N (update N i inc))))))


(defn calc-t-AO
  [e d n & {:keys [c]
            :or {c 1}}]
  (/ (* -1 (Math/log d) (math/square (Math/log n)) c) (math/square e)))

(defn calc-e-d-eps-AO
  "Returns epsilon and delta"
  [n k sigma]
  (let [e (Math/pow (/ k n) (/ 1 3))
        d (/ 1 k n n)]
    [e
     d
     (/ (* k (Math/log d) (math/square (Math/log n)) -4 (math/square sigma))
        n (math/square e))]))


;; (defn GESMR
;;   "From the paper Effective Mutation Rate Adaptation Through Group Elite Selection"
;;   [n meta-sigma & {:keys [k]}]
;;   (let [k (or k (int (math/pow n (/ 3 4))))
;;         k (int (/ n (math/round (/ n k))))
;;         init-pop ]
;;     {}))

(defn max-bandit
  "A bandit has a base coding representing its highest resolution, 
   and then a set of num-codings tile codings that learn the weights 
   associated with their tiles. 
   At inference, all tiles in the base-coding are scored using the other
   tile-codings, and based on the sampling mode, d high-res tiles are returned"
  [l r to ta num-codings d acc delta n mode maximize? & {:keys [AO xi* s epsilon lr special temperature dampening momentum-lr max-n dist-acc kde bandwidths legacy sigma]
                                                         :or {epsilon [0] temperature [50] momentum-lr [0.1]}}]
  (let [xi* (rand-nth xi*)
        base-coding (tile-coding l r (mapv (constantly [0]) acc) (mapv vector acc) :max-bandit true :AO AO :special special :n n :xi* xi* :max-n max-n :dist-acc dist-acc :kde kde)
        delta (rand-nth delta)
        lr (rand-nth lr)]
    (merge
     {:tile-codings (when-not kde (into [] (repeatedly num-codings #(tile-coding l r to ta :max-bandit true :AO AO :special special :n n :xi* xi* :momentum? momentum-lr :max-n max-n :lr lr))))
      :base-coding (if kde
                     base-coding
                     (assoc (dissoc base-coding :m)
                            :w (double-array (:m base-coding))
                            :N (long-array (:N base-coding))))
      :d (rand-nth d)
      :n n
      :mode (rand-nth mode)
      :delta delta
      :max-bandit true
      :t ##-Inf
      :max-reward ##-Inf
      :maximize? maximize?
      :epsilon (rand-nth epsilon)
      :AO AO
      :lr lr
      :temperature (rand-nth temperature)
      :generation 0
      :dampening dampening
      :momentum-lr (rand-nth momentum-lr)
      :max-n max-n
      :kde kde
      :dist-acc dist-acc
      :bandwidths bandwidths
      :sigma (or sigma 0)
      :legacy legacy}
     (if AO
       {:xi* xi*}
       {:s (rand-nth s)
        :top []}))))

#_(max-bandit [0 0] [1 1]
              [[0.03 0.05 0.07] [0.03 0.05 0.07]]
              [[0.2 0.3 0.4] [0.2 0.3 0.4]]
              1
              [3]
              [0.1 0.1]
              [0.999 0.99 0.95]
              100
              [:sample :argmax]
              false
              :s [3 6 9]
              :lr [0.001 0.01 0.1]
              :AO true
              :xi* [0.01]
              :epsilon [0.01 0.1])

(defn max-from-distribution
  "Calculates the expected maximum over n draws from a given pdf with given accuracy and x-position"
  [^doubles dist n dist-acc ^doubles dist-pos]
  (let [ndec (dec n)
        total (* dist-acc (areduce dist idx ret 0. (+ ret (aget dist idx))))
        ^doubles dist (amap dist idx ret (/ (aget dist idx) total))
        ^doubles cdf (areduce dist idx ^doubles ret
                              (make-array Double/TYPE (alength dist))
                              (let [prev (if (zero? idx) 0 (aget ret (dec idx)))
                                    total (+ prev (* dist-acc (aget dist idx)))]
                                (aset ret idx total)
                                ret))
        ^doubles integrand (amap cdf idx ^doubles _ (* n
                                                       (aget dist-pos idx)
                                                       dist-acc
                                                       (math/pow (aget cdf idx) ndec)
                                                       (aget dist idx)))]
    (areduce integrand idx ret 0. (+ ret (aget integrand idx)))))

(defn amap-test
  []
  (let [dist-acc 0.01
        low -3
        high 3
        x (double-array (map #(+ (* % dist-acc) low) (range (int (/ (- high low) dist-acc)))))
        arr (amap x idx ret ^double (/ (math/exp (- (/ (math/square (aget x idx)) 2))) (math/sqrt (* 2 3.14159265))))
        _ (pprint/pprint arr)
        avg (max-from-distribution arr 4 dist-acc x)]
    ;; (println avg)
    (pprint/pprint (mapv #(max-from-distribution arr % dist-acc x) (range 1 20)))))

(defn evaluate-coding-kde
  "Update the weights in a coding by setting them to the expected max over n draws from each distribution"
  [coding n]
  (let [{^"[[D" dist :dist
         dist-pos :dist-pos
         ^doubles w :w
         dist-acc :dist-acc
         ^doubles N :N
         evaluated :evaluated} coding]
    (if (not (seq evaluated))
      coding
      (do
        (mapv #(aset w % ^double (max-from-distribution (aget dist %)
                                                        n
                                                        dist-acc
                                                        dist-pos))
              evaluated)
        (assoc coding :evaluated #{})))))

(defn evaluate-tile-distribution-free
  [tile tile-codings & {:keys [t]}]
  (let [middle (mapv mean tile)
        indices (mapv (partial to-tile-index middle) tile-codings)]
    {:w (mean (map #(count (filter (partial < t) (nth (:w %1) %2 []))) tile-codings indices))
     :N (mean (map #(nth (:N %1) %2 0) tile-codings indices))}))

;;If tile wasn't updated, no need to reevaluate
(defn evaluate-tile-AO
  [tile tile-codings]
  (let [middle (mapv #(if (coll? %) (math/mean %) %) tile)
        indices (mapv (partial to-tile-index middle) tile-codings)
        ;; indices (mapv #(to-tile-index (if (coll? %1) (math/mean %1) %1) %2) tile tile-codings)
        n (count indices)]
    {:w (/ (transduce (map #(aget ^doubles (:m (nth tile-codings %)) (nth indices %))) + (range n)) n)
     :N (/ (transduce (map #(aget ^longs (:N (nth tile-codings %)) (nth indices %))) + (range n)) n)}
    #_{:w (mean (map #(aget ^doubles (:m %1) %2) tile-codings indices))
       :N (mean (map #(aget ^longs (:N %1) %2) tile-codings indices))}))


(defn evaluate-tile
  [tile tile-codings & {:keys [AO t max-n]}]
  (if (or max-n AO)
    (evaluate-tile-AO tile tile-codings)
    (evaluate-tile-distribution-free tile tile-codings :t t)))


(defn evaluate-bandit
  [bandit & {:keys [indices mapper] :or {mapper mapv}}]
  ;; (println "eval bandit time")
  (let [{tile-codings :tile-codings
         {tiles :tiles
          ^doubles w :w
          ^longs N :N
          :as base-coding} :base-coding
         t :t
         top :top
         s :s
         n :n
         AO :AO
         delta :delta
         max-bandit :max-bandit
         kde :kde
         max-n :max-n} bandit]
    (if kde
      (assoc bandit :base-coding (evaluate-coding-kde base-coding n))
      (let [^longs indices (identity (if (nil? indices)
                                       (long-array (range (count tiles)))
                                       (long-array indices)))
              ;; got-indexes (time (mapv (partial nth tiles) indices))
            ^"[Lclojure.lang.PersistentArrayMap;" evaluated (identity (into-array (mapper #(evaluate-tile (nth tiles %)
                                                                                                          tile-codings
                                                                                                          :max-bandit max-bandit
                                                                                                          :t t
                                                                                                          :AO AO
                                                                                                          :delta delta
                                                                                                          :max-n max-n)
                                                                                          indices)))]
        (areduce indices id-idx _ nil
                 (let [id (aget indices id-idx)
                       e (aget evaluated id-idx)]
                   (aset-double w id (:w e))
                   (aset-long N id (:N e))))
        (merge bandit
               (when-not (or max-n AO)
                 {:t (if max-bandit
                       (max t (+ (nth (sort > top) (dec s) ##-Inf) E))
                       t)}))))))


;; (let [arr1 (into #{} (range 1000))
;;       arr2 (double-array (range 1000))]
;;   (time (dotimes [_ 1000]
;;           (mapv (fn [id e] (aset-double arr2 id e))
;;                 (range 1000)
;;                 arr1))))

;; (let [arr1 (into #{} (range 1000))
;;       arr2 (double-array (range 1000))]
;;   (time (let [arr1 (double-array arr1)]
;;           (dotimes [_ 1000]
;;           (areduce arr1 id ret nil (aset-double arr2 id (aget arr1 id)))))))

;; (let [arr1 (double-array (range 1000))
;;       arr2 (double-array (range 1000))]
;;   (time (dotimes [_ 1000]
;;           (dotimes [i (alength arr2)]
;;             (aset-double arr2 i (aget arr1 i))))))

(defn sigmoid [x] (/ 1 (+ 1 (Math/exp (- x)))))


(defn update-bandit-single
  [x r bandit & {:keys [V mapper] :or {mapper mapv}}]
  (let [{tile-codings :tile-codings
         base-coding :base-coding
         lr :lr
         top :top
         t :t
         s :s
         max-bandit :max-bandit
         AO :AO
         xi* :xi*
         max-reward :max-reward
         n :n
         generation :generation
         dampening :dampening
         momentum-lr :momentum-lr
         kde :kde
         bandwidths :bandwidths
         max-n :max-n} bandit
        lr (cond (or (vector? lr) (instance? (class (double-array [])) lr)) (nth lr generation)
                 (map? lr) (assoc lr :t generation)
                 :else lr)
        ;; r (if maximize? r (- (Math/log r)))
        top (if (> r t) (conj top r) top)
        max-reward (max max-reward r)
        r (if AO (+ r (* E (rand-normal 0 1))) r)]
    (assert (if max-bandit t V) "If max-bandit, must have t. If not, must have v")
    (if (nil? x)
      bandit
      (merge bandit
             {:tile-codings (mapper #(update-coding x r % lr
                                                    :max-bandit max-bandit
                                                    :t t
                                                    :V V
                                                    :n n
                                                    :AO AO
                                                    :xi* xi*
                                                    :dampening dampening
                                                    :momentum-lr momentum-lr)
                                    tile-codings)
              :max-reward max-reward}
             (when-not (or max-n AO)
               {:top (filter (partial < t) top)
                :t (max t (+ (nth (sort > top) (dec s) ##-Inf) E))})
             (when kde {:base-coding (update-coding-kde x r base-coding lr :bandwidths bandwidths)})))))




(defn bandit-indices-single
  [x bandit]
  (let [{base-coding :base-coding
         tile-codings :tile-codings} bandit
        get-base-indices (fn [tile] [(to-tile-index (map #(- (if (coll? %) (first %) %) E) tile) base-coding)
                                     (to-tile-index (map #(- (if (coll? %) (second %) %) E) tile) base-coding)])
        tile-indices (map (partial to-tile-index x) tile-codings)
        ttb-indices (map-indexed #(get-base-indices (nth (:tiles (nth tile-codings %1)) %2))
                                 tile-indices)
        ttb-indices (flatten ttb-indices)]
    [(apply min ttb-indices)
     (apply max ttb-indices)]))

(defn update-bandit
  [x r bandit & {:keys [V mapper] :or {mapper mapv}}]
  (let [indices (map #(bandit-indices-single % bandit) x)
        indices (map #(range (first %) (inc (second %))) indices)
        indices (reduce into #{} indices)]
    (evaluate-bandit
     ((apply comp
             (map #(fn [b]
                     (update-bandit-single %1 %2 b :V %3))
                  x r (or V (repeat nil))))
      bandit)
     :indices indices)))


(defn update-coding-parallel
  "Uses learning rate to update the max-reward and num-visitation
   of each tile in the coding \\
   Maybe should use the GDI formuation w+=lr(g-V) instead?
   lr is close to 0 in the GDI formulation, but close to 1 in the max-bandit formulation"
  [x r coding lr & {:keys [max-bandit AO V t n xi* dampening momentum-lr max-n kde bandwidths legacy]}]
  (cond kde ((apply comp
                    (map #(fn [c]
                            (update-coding-kde %1 %2 c lr :bandwidths bandwidths))
                         x r))
             coding)
        max-n ((apply comp
                      (map #(fn [c]
                              (update-coding-maxn %1 %2 c lr :dampening dampening :momentum-lr momentum-lr :legacy legacy))
                           x r))
               coding)
        AO
        ((apply comp
                (map #(fn [c]
                        (update-coding-AO %1 %2 c lr n xi* :dampening dampening :momentum-lr momentum-lr))
                     x r))
         coding)
        :else (throw (Exception. "Parallel implementation only available for AO and max-n bandits"))))

(defn update-bandit-parallel
  [x r bandit & {:keys [V mapper] :or {mapper mapv}}]
  (let [{tile-codings :tile-codings
         base-coding :base-coding
         lr :lr
         top :top
         t :t
         s :s
         max-bandit :max-bandit
         AO :AO
         xi* :xi*
         max-reward :max-reward
         n :n
         generation :generation
         dampening :dampening
         momentum-lr :momentum-lr
         max-n :max-n
         kde :kde
         bandwidths :bandwidths
         legacy :legacy} bandit]
    (if kde
      (evaluate-bandit (assoc bandit :base-coding (update-coding-parallel x r base-coding lr :bandwidths bandwidths :kde true)))
      (identity (let [;;_ (println "indices time")
                      indices (if (< (count (:w base-coding)) 1000)
                                nil
                                (let [idx (identity (mapv #(bandit-indices-single % bandit) x))
                                      idx (mapv #(range (first %) (inc (second %))) idx)
                                      idx (reduce into #{} idx)]
                                  idx))
                      ;;_ (pprint/pprint {:num-indices (count indices)})
                      lr (cond (or (vector? lr) (instance? (class (double-array [])) lr)) (nth lr generation)
                               (map? lr) (assoc lr :t generation)
                               :else lr)
                      non-nil (filter (partial nth x) (range (count x)))
                      x (mapv (partial nth x) non-nil)
                      r (mapv (partial nth r) non-nil)
                      top (if (> (apply max r) t) (into [] (concat top (filter (partial < t) r))) top)
                      max-reward (apply max max-reward r)
                      r (if AO (mapv #(+ % (* E (math/abs (rand-normal 0 1)))) r) r)
                      ;; _ (println "update time")
                      codings (identity (mapper #(update-coding-parallel x r % lr
                                                                         :max-bandit max-bandit
                                                                         :t t
                                                                         :V V
                                                                         :n n
                                                                         :AO AO
                                                                         :xi* xi*
                                                                         :dampening dampening
                                                                         :momentum-lr momentum-lr
                                                                         :max-n max-n
                                                                         :legacy legacy)
                                                tile-codings))]
                  (assert (if max-bandit t V) "If max-bandit, must have t. If not, must have v")
                  ;; (println "bandit evaluation time")
                  (identity (evaluate-bandit
                             (if (nil? x)
                               bandit
                               (merge (assoc bandit
                                             :tile-codings codings
                                             :max-reward max-reward)
                                      (when-not AO
                                        {:top (filter (partial < t) top)
                                         :t (max t (+ (nth (sort > top) (dec s) ##-Inf) E))})))
                             :mapper mapper
                             :indices indices)))))))



;;For each tile in the base-coding, it's the max of the weights and the sum of the Ns

#_(def b (max-bandit [0]
                     [1]
                     [[0.1 0.2]]
                     [[0.2]]
                     2
                     3
                     [0.1]
                     0.99
                     3
                     0.1
                     1000
                     :sample
                     false))

;; b
;; (evaluate-bandit b)

;; (evaluate-bandit 
;;  (update-bandit [[1]] [10]
;;   (update-bandit [[0.15]] [1] b)))

#_(defn scale-softmax
    [coll]
    (loop [coll coll]
      (let [mi (apply min coll)
            mii (apply min (filter (partial < mi) coll))
            ma (apply max coll)]
        (if (< (- ma mii) 500)
          (if (< (- ma mi) 500) coll (map (partial min 500) coll))
          (recur (map #(max 0 (- % mii)) coll))))))

#_(defn softmax
    [coll & {:keys [temperature]
             :or {temperature 1}}]
    (let [coll (scale-softmax coll)
          coll (mapv #(/ % temperature) coll)
          s (mapv #(Math/exp %) coll)
          sum (reduce + s)]
      (if (zero? sum)
        (mapv (constantly 0) s)
        (mapv #(max (- (/ % sum) E) 0) s))))

(defn asoftmax
  "Array implementation of softmax"
  [^doubles coll  & {:keys [temperature]
                     :or {temperature 1}}]
  (let [^doubles coll (amap coll idx ret (Math/exp (/ (aget coll idx) temperature)))
        s (areduce coll idx ret 0. (+ ret (aget coll idx)))]
    (if (zero? s)
      (double-array (alength coll))
      (amap coll idx ret (/ (aget coll idx) s)))))

(defn gumbel
  []
  (- (Math/log (- (Math/log (rand 1))))))

(defn gumbel-max
  [coll & {:keys [argsort]}]
  (let [n (count coll)
        shifted (map + coll (repeatedly n gumbel))]
    ((if argsort
       #(sort-by %1 > %2)
       (partial apply max-key))
     (partial nth shifted) (range n))))

(defn update-TA-encoding
  "Updates encoding for threshold ascent"
  [x r coding lr t]
  (let [{w :w
         N :N} coding
        i (to-tile-index x coding)]
    (assoc coding
           :w (update w i
                      #(filterv (partial < t)
                                (conj (map
                                       (partial * lr)
                                       %)
                                      r)))
           :N (into [] (map-indexed #(if (= i %1)
                                       (ema 1 %2 lr)
                                       (ema 0 %2 lr))
                                    N)))))

(defn U
  [u0 n0 n k d]
  (let [a (Math/log (/ (* 2 n k) d))]
    (if (> n0 0)
      (+ u0
         (/ (+ a (Math/sqrt (+ (* 2 n0 u0 a)
                               (math/square a))))
            n0))
      1000000000)))

(defn _U
  [s0 n0 n k d]
  (if (> n0 0)
    (U (/ s0 n0) n0 n k d)
    10000000000))

;; (_U 3 10 100 20 0.01)

#_(U 1 2 100 5 0.01)

(defn S
  [payoffs t]
  (count (filter (partial < t) payoffs)))

(defn update-max-bandit-TA
  [i r t TA]
  (let [{N :N
         w :w} TA]
    (assoc TA
           :N (update N i inc)
           :w (update w i #(filterv (partial < t) (conj % r))))))

(defn sample-TA
  "Sampling of a bandit arm according to threshold ascent\\
   I should use like a composite tile-coding that has a higher resolution
   than all the actually updated codings\\
   Returns the sampled arm and the udpated t"
  [TA s n d t]
  (let [{N :N
         w :w} TA
        k (count N)
        s-values (map #(S % t) w)
        u (mapv #(_U %1 %2 n k d) s-values N)]
    [(apply max-key #(u %) (range k))
     (max t (+ (nth (sort > (flatten w)) (dec s) ##-Inf) E))]))



#_(sample-TA {:N [2 3 100 5]
              :w [[1 2 3 4]
                  [0 1 2 3]
                  [-1 0 1 2]
                  [-2 -1 0 1]]}
             3
             100
             0.01
             2)



(defn simulate-max-bandit-ta
  [bandits n s d & {:keys [update-fn sample-fn bandit-update verbose]
                    :or {update-fn update-max-bandit-TA
                         sample-fn sample-TA
                         bandit-update identity}}]
  (let [k (count bandits)]
    (loop [TA {:w (into [] (repeat k []))
               :N (into [] (repeat k 0))}
           bandits bandits
           t ##-Inf
           _n 0]
      (if (= _n n)
        {:TA TA
         :t t
         :max (apply max (flatten (:w TA)))}
        (let [[i t] (sample-fn TA s n d t)
              [mean std] (nth bandits i)
              r (rand-normal mean std)
              TA (update-fn i r t TA)]
          (when verbose
            (println "Sampled reward " r " from bandit " i)
            (println "TA " TA))
          (recur TA (bandit-update bandits) (double t) (inc _n)))))))

(set! *warn-on-reflection* true)

(defn random-mutate-tile
  [tile sigma max-tile min-tile]
  (max min-tile (min max-tile (int (+ tile (rand-normal 0 sigma))))))


;;Need to have epsilon-greedy for 
(defn sample-bandit
  [bandit]
  (let [{d :d
         delta :delta
         mode :mode
         {^doubles w :w ;s-values
          N :N
          tiles :tiles
          r :r
          num-tiles :num-tiles} :base-coding
         temperature :temperature
         n :n
         AO :AO
         epsilon :epsilon
         generation :generation
         max-n :max-n
         kde :kde
         sigma :sigma} bandit
        epsilon (if (number? epsilon) epsilon (nth epsilon generation))
        temperature (if (number? temperature) temperature (nth temperature generation))
        k (count N)
        ^doubles u (if (or max-n AO) (aclone w) (mapv #(_U %1 %2 n k delta) w N))
        scores (condp = mode
                 :argmax u
                 :softmax (amap u u-idx _ (+ (* temperature (aget u u-idx)) (gumbel)))
                 :softmax-scaled (let [e (/ (areduce u idx ret 0. (+ ret (aget u idx))) (alength u))
                                       e2 (areduce u idx ret 0. (+ ret (math/square (- (aget u idx) e))))
                                       std (math/sqrt (/ e2 (dec (alength u))))]
                                   (amap u u-idx _ (+ (/ (* temperature (aget u u-idx)) (+ 1E-20 std)) (gumbel))))
                 :proportional (amap u u-idx _ (double (+ (* temperature (math/log (aget u u-idx))) (gumbel))))
                 :xex (amap u u-idx _ (double (+ (* temperature (#(+ (math/abs %) (math/log (math/abs %))) (aget u u-idx))) (gumbel))))
                 :xex2 (amap u u-idx _ (double (+ (#(+ (* temperature (math/abs %)) (math/log (math/abs %))) (aget u u-idx)) (gumbel)))))
        indices (try (take d (sort-by (partial aget scores) > (range k)))
                     (catch Exception e (do (pprint/pprint {:scores scores}) (with-out-str (pprint/pprint {:scores scores})))))]
    (mapv (comp
           (if kde
             #(mapv vector (nth tiles %) (nth tiles (inc %) r))
             (partial nth tiles))
           #(random-mutate-tile % sigma (dec k) 0)
           #(if (< (rand) epsilon) (rand-int k) %))
          indices)))



#_(println "scores length " (count scores) " type " (type scores) " k " k  " % " % " value " (nth scores %) " values " (into [] scores))
#_(nth (double-array [1.7969878774470686E-5 0.0010088853880527398 1.211123819119977E-5 0.0029006121294014036 0.008829689790464852 4.794247360719063E-6 1.1986672872798495E-5 0.010961219412463 6.639857985204241E-4 6.94847853225117E-6 9.386666479861497E-5 0.0058553013230432235 2.9474495913653224E-5 0.7327285217310552 0.17209086767265275 1.3055412206309343E-5 0.06481381512429864 0.11819008031541153 0.007550071067522511 0.017809129604678106 0.19841971272909736 1.1602950231964112E-6 8.037179669253614E-4 5.307869948717807E-4 2.440806453452589E-5 0.0030155270717055808])
       2)

;; (sample-bandit (evaluate-bandit (update-bandit [[0] [0.5] [1]] [-100 -100 -99] b)))
;; (sample-bandit b)

#_(let [uniform-sample (fn [TA s n d t] [(rand-int (count (:w TA))) t])
        niter 1000
        sampler (partial simulate-max-bandit-ta
                         [[-5 1]
                          [-5 1]
                          [0 10]]
                         10
                         4
                         0.01
                         #_:bandit-update #_(partial mapv #(vector (ema (rand-normal 0 1) (first %) 0.9)
                                                                   (ema (rand-normal 0 1) (second %) 0.9))))
        t1 (repeatedly niter sampler)
        t2 (repeatedly niter #(sampler :sample-fn uniform-sample))]
    ((math/mean (map :max t1)))
    ((math/mean (map :max t2))))

;Selection pressure temperature e^t from -50 (0) to 50
;test case temperature
;test case weights -- really fucking big space
;UMAD rate from 0 to 1


;; (defn exponential-sampling
;;   [[low high]]
;;   (Math/exp (+ low (rand (- high low)))))

(defn exponential-sampling
  [[low high] & {:keys [l r sigma]}]
  (let [l (or l low)
        r (or r high)
        sigma (or sigma 0)
        unif (+ low (rand (- high low)))
        mutated (+ unif (rand-normal 0 sigma))
        x (min r (max l mutated))]
    (Math/exp x)))

(defn tile-to-parameters-umad
  [tile & {:keys [low high sigma]}]
  {:e (exponential-sampling (first tile) :l low :r high :sigma sigma)})

(defn tile-to-parameters-0
  "Selection pressure temperature = e^t \\
   UMAD rate = e (1/e^t-1)?"
  [tile]
  (if (coll? (first tile))
    (let [[t e] (map exponential-sampling tile)]
      {:t t
       :e e})
    {:t (first tile)
     :e (exponential-sampling (second tile))}))


(defn tile-to-parameters-1
  "Selection pressure temperature \\
   test case temperature\\
   test case weights \\
   UMAD rate"
  [tile]
  (let [[t-s t-t] tile
        [t-s t-t] (map exponential-sampling [t-s t-t])
        t-w (map (partial apply sample-uniform)
                 (drop-last (drop 2 tile)))
        e (exponential-sampling (last tile))]
    {:t-s t-s
     :t-t t-t
     :t-w t-w
     :e e}))

(defn get-value
  [^doubles weights ^long idx]
  (let [n (alength weights)]
    (loop [i 0
           ret 0.
           idx idx]
      (if (= i n)
        ret
        (recur (inc i)
               (if (= (rem idx 2) 0)
                 (+ ret (aget weights i))
                 ret)
               (int (/ idx 2)))))))

(defn get-weights
  [^doubles weights ^long num-weights]
  (let [new-weights (double-array num-weights)]
    (loop [i 0]
      (if (= i num-weights)
        new-weights
        (do (aset-double new-weights i (get-value weights i))
            (recur (inc i)))))))

(defn exponential-mapping
  [weights & {:keys [num-cases]
              :or {num-cases 200}}]
  (let [weights (if (instance? (Class/forName "[D") weights)
                  weights
                  (double-array weights))]
    (get-weights weights num-cases)))

(defn tile-to-parameters-exp
  "Exponential - uses logn features to identify one thing"
  [tile & {:keys [num-cases]
           :or {num-cases 200}}]
  (let [[t-s t-t & t-w] tile
        [t-s t-t] (map exponential-sampling [t-s t-t])
        e (exponential-sampling (last t-w))
        -t-w (map (partial apply sample-uniform)
                  (drop-last t-w))
        t-w (exponential-mapping
             -t-w
             :num-cases num-cases)]
    {:t-s t-s
     :t-t t-t
     :t-w t-w
     :-t-w -t-w
     :e e}))


(defn tile-to-parameters-random
  "Uses a set of normally distributed random basis vectors"
  [tile & {:keys [basis lexicase low high sigma]}]
  (let [low (or low (repeat (count tile) nil))
        high (or high (repeat (count tile) nil))
        sigma (or sigma (repeat (count tile) nil))
        tile (map #(sample-uniform (first %1) (second %1) :low %2 :high %3 :sigma %4) tile low high sigma)
        weights (apply math/aplus (map #(amap ^doubles %1 idx _ (* (aget ^doubles %1 idx) %2)) basis tile))]
    {:weights (math/softmax weights)
     :tile tile
     :lexicase lexicase}))

(defn tile-to-parameters-random-umad
  "Uses a set of normally distributed random basis vectors"
  [tile & {:keys [basis lexicase low high sigma]}]
  (let [low (or low (repeat (count tile) nil))
        high (or high (repeat (count tile) nil))
        sigma (or sigma (repeat (count tile) nil))
        sampled-tile (map #(sample-uniform (first %1) (second %1) :low %2 :high %3 :sigma %4) tile low high sigma)
        weights (apply math/aplus (map #(amap ^doubles %1 idx _ (* (aget ^doubles %1 idx) %2)) basis sampled-tile))]
    {:weights (math/softmax weights)
     :tile sampled-tile
     :lexicase lexicase
     :e (math/exp (first sampled-tile))}))

;; (defn ttp-test
;;   []
;;   (let []
;;     ()))



(defn parameters-to-tile-0
  "Selection pressure temperature = 1/(e^t-1) \\
   UMAD rate = e"
  [parameters]
  (when-not (nil? parameters)
    (let [{t :t
           e :e} parameters]
      [(if (number? t) (Math/log t) t)
       (if (number? e) (Math/log e) e)])))

(defn parameters-to-tile-1
  [parameters]
  (when-not (nil? parameters)
    (let [{t-s :t-s
           t-t :t-t
           t-w :t-w
           e :e} parameters
          log #(if (number? %) (Math/log %) %)]
      (concat [(log t-s) (log t-t)]
              t-w
              [(log e)]))))

(defn parameters-to-tile-exp
  [parameters]
  (when-not (nil? parameters)
    (let [{t-s :t-s
           t-t :t-t
           -t-w :-t-w
           t-w :t-w
           e :e} parameters
          log #(if (number? %) (Math/log %) %)]
      (concat
       [(log t-s) (log t-t)]
       -t-w
       [(log e)]))))

(defn parameters-to-tile-umad
  [parameters]
  (when-not (nil? parameters)
    [(#(if (number? %) (Math/log %) %) (:e parameters))]))

(defn parameters-to-tile-random
  [parameters & {:keys [basis]}]
  (let [{tile :tile} parameters]
    tile))

#_(defn sample-AO
    "Asymptotically optimal - assumes GEV\\
   Returns index of arm to sample"
    [AO n & {:keys [c]
             :or {c 1}}]
    (let [{w :w
           N :N} AO
          k (count w)
          [e d] (calc-e-d-AO n k)
          eps (/ (* k (calc-t-AO e d n :c c)) n)]
      (if (< (rand) eps)
        (rand-int k)
        ())))


;1/r or log(r)?
(defn softmax-selection
  [pop parameters initializer]
  (let [{t :t} parameters
        parent (if (number? t)
                 (gumbel-max
                  (map #(/ (- (Math/log %)) t)
                       (map :total-error pop)))
                 (initializer))]
    (assoc (nth pop parent) :bandit-parameters parameters)))

;;would have to edit errors here
#_(defn weighted-selection
    [pop parameters initializer]
    (let [{t-s :t-s
           t-t :t-t
           t-w :t-w} parameters
          t-w (asoftmax t-w :temperature t-t)]
      (if (number? t-s)
        (nth pop (gumbel-max
                  (map #(/ (reduce - 0 (map * t-w %)) t-s)
                       (map :errors pop))))
        (initializer))))

(defn weighted-selection
  [pop parameters initializer]
  (let [{:keys [t-s t-t t-w]
         :or {t-s 1 t-t 1}} parameters
        ^doubles t-w (asoftmax t-w :temperature t-t)]
    (if (number? t-s)
      (nth pop (gumbel-max
                (map (fn [^doubles err]
                       (let [^doubles err (amap err idx _ (* (aget err idx) (aget t-w idx)))]
                         (/ (areduce err idx ret 0. (+ (aget err idx) ret))
                            t-s)))
                     (map :errors pop))))
      (initializer))))

(defn selection
  "Select an individual from the population based on the supplied parameters"
  [pop parameters initializer argmap]
  (assoc
   (cond
     (:lexicase parameters) (selection/lexicase-selection pop argmap :cases (gumbel-max (:t-w parameters) :argsort true) :maximize true)
     (not (nil? (:t parameters))) (softmax-selection pop parameters initializer)
     (not (nil? (:t-s parameters))) (weighted-selection pop parameters initializer)
     :else (selection/select-parent pop argmap))
   :bandit-parameters parameters))

(defn umad
  "Vary an individual's genome based on its parameters using UMAD. 
   Parameters are of the form {:e umad-rate}"
  [individual argmap]
  (let [{{e :e
          weights :weights
          :as b} :bandit-parameters
         p :plushy} individual
        {:keys [instructions umad-rate]} argmap
        e (or e umad-rate)
        keep-rate (/ 1 (inc e))]
    {:plushy (if (< e 5)
               (-> p
                   (variation/uniform-addition instructions e)
                   (variation/uniform-deletion e))
               (into [] (mapcat identity
                                (map (fn [instr]
                                       (shuffle
                                        (concat
                                         (when (< (rand) keep-rate) [instr])
                                         (repeatedly (dist/rand-binomial (variation/rand-round e) keep-rate) #(utils/random-instruction instructions)))))
                                     p))))
     :bandit-parameters b}))

(defn new-individual
  "Create a new individual using supplied parameters
   Supply a single individual to pop and indicate preselected? to only use variation"
  [pop argmap parameters initializer & {:keys [preselected? samr?]}]
  (cond
    samr? (assoc (umad pop argmap) :bandit-parameters {:e (variation/meta-mutate-gsemr (:e (:bandit-parameters pop)) 2)})
    preselected? (umad (assoc pop :bandit-parameters parameters) argmap)
    :else (umad (selection pop parameters initializer argmap) argmap)))

(defn bandit-ensemble
  "Ensemble multiple max-bandits together\\
   NEED TO IMPLEMENT RANDOMNESS"
  [& {:keys [num-bandits l r to ta num-codings d acc delta n mode maximize? AO xi* s lr epsilon special temperature dampening momentum-lr max-n kde dist-acc bandwidths legacy sigma]}]
  (into []
        (repeatedly num-bandits
                    #(max-bandit l
                                 r
                                 to
                                 ta
                                 num-codings
                                 d
                                 acc
                                 delta
                                 n
                                 mode
                                 maximize?
                                 :AO AO
                                 :lr lr
                                 :s s
                                 :epsilon epsilon
                                 :special special
                                 :xi* xi*
                                 :temperature temperature
                                 :dampening dampening
                                 :momentum-lr momentum-lr
                                 :max-n max-n
                                 :dist-acc dist-acc
                                 :kde kde
                                 :sigma sigma
                                 :bandwidths bandwidths
                                 :legacy legacy))))


#_(max-bandit [0 0] [1 1]
              [[0.03 0.05 0.07] [0.03 0.05 0.07]]
              [[0.2 0.3 0.4] [0.2 0.3 0.4]]
              1
              [3]
              [0.1 0.1]
              [0.999 0.99 0.95]
              [3 6 9]
              [0.001 0.01 0.1]
              100
              [:sample :argmax]
              false)


#_(def be (apply bandit-ensemble (into [] (mapcat identity {:num-bandits 3
                                                            :l [-5 0]
                                                            :r [5 1]
                                                            :to [[0 0.2 0.5 1]
                                                                 [0 0.02 0.05 0.1]]
                                                            :ta [[0.2 0.3 0.4]
                                                                 [0.02 0.03 0.04]]
                                                            :num-codings 3
                                                            :d [4]
                                                            :acc [0.1 0.01]
                                                            :lr [0.999 0.99 0.95 0.9]
                                                            :s [5]
                                                            :delta [0.01]
                                                            :n 100;timescale of the changing of the maxbandit distribution
                                                            :mode [:argmax :sample]
                                                            :maximize? true}))))


(defn sample-bandit-ensemble
  "Sample from each bandit, and then pick a random sampled parameter set"
  [bandits & {:keys [transformation]
              :or {transformation tile-to-parameters-0}}]
  (transformation (rand-nth (mapcat sample-bandit bandits))))

(defn update-bandit-ensemble
  "Update each bandit in an ensemble using the returns of multiple sampled selectors"
  [x r bandits & {:keys [transformation mapper parallel]
                  :or {transformation parameters-to-tile-0
                       mapper mapv}}]
  (if parallel
    (mapv #(update-bandit-parallel (map transformation x) r % :mapper mapper) bandits)
    (mapper #(update-bandit (map transformation x) r % :mapper mapv) bandits)))

#_(update-bandit-ensemble
   [{:t 1.03 :e 0.14}] [100] be)

(defn print-bandits
  [bandits]
  (let [update-print (fn [bandit] (update (update bandit :base-coding #(dissoc % :tiles :mi))
                                          :tile-codings #(mapv (fn [c] (dissoc c :w :tiles)) %)))]
    (pprint/pprint {:bandits (mapv update-print bandits)})))

(defn print-frequencies
  [bandits]
  (let [update-print (fn [bandit] (update (update bandit :base-coding #(dissoc % :tiles :mi))
                                          :tile-codings #(mapv (fn [c] (dissoc c :w :tiles)) %)))
        get-n (fn [bandit] (:w (:base-coding bandit)))]
    (pprint/pprint (mapv get-n (mapv update-print bandits)))))


(defn trim-bandit
  [bandit & {:keys [succinct]}]
  (let [{tile-codings :tile-codings base-coding :base-coding} bandit]
    (if succinct
      {:base-coding (dissoc base-coding :tiles :dist :n-since-update)}
      (assoc (dissoc bandit :epsilon :temperature :top)
             :tile-codings (mapv #(if (:max-n %) (assoc (dissoc % :tiles :mi :n-since-update) :w  (double-array (mapv (partial apply max) (:w %)))) (dissoc % :tiles :w)) tile-codings)
             :base-coding (dissoc base-coding :tiles :dist :n-since-update)))))

(defn prettify-dist
  [dist & {:keys [succinct]}]
  (let [{groups :groups probs :probabilities rew :rewards parameter-rewards :parameter-rewards parameters :parameters} dist]
    (merge {:probabilities (into {} (mapv #(vector (first %) (double (second %))) probs))
            :rewards (double-array rew)}
           (when-not succinct
             {;;:parameters (double-array parameters)
              :parameter-rewards (into {} (mapv #(vector (str (first %)) (double-array (second %))) parameter-rewards))}))))

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
                              (when-let [s (:sigma argmap)] {:sigma s})
                              (when dist {:dist (prettify-dist dist :succinct succinct)})
                              (when rates {:rates (double-array rates)})
                              #_(when bandits {:bandits (mapv #(trim-bandit % :succinct succinct) bandits)}))))
    (println)))

(def error-transformations
  {:nlog #(- (Math/log (max % (Math/exp -5))))
   :identity identity
   :inv #(/ 1 (max 1E-6 %))
   :neg -})

(defn get-error-transformation
  [func-key & {:keys [array-errors]}]
  (let [func (get error-transformations func-key)]
    (fn [ind]
      (let [total (:total-error ind)
            errors  (mapv func (:errors ind))]
        (merge ind
               {;;:errors errors 
                :total-error- total
                :total-error (reduce + errors)})))))


(def reward-transformations-builder
  "Unique reward transformations for max bandits."
  {:diff (fn [ind parent dist]
           (- (math/mean (:errors parent))
              (math/mean (:errors ind))))
   :selection-prob-diff (fn [ind parent dist]
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
  ;;  :log-diff (fn [ind parent _]
  ;;              (- (math/mean (map #(math/log (inc %)) (:errors parent)))
  ;;                 (math/mean (map #(math/log (inc %)) (:errors ind)))))
   :log-diff (fn [ind parent _]
               (let [ei (:errors ind) ep (:errors parent)]
                 (/ (- (transduce (map #(math/log (inc %))) + ep)
                       (transduce (map #(math/log (inc %))) + ei)) (count ei))))
   :safe-log-diff (fn [ind parent _]
                    (- (math/mean (map #(* (math/sign %) (math/log (+ E (math/abs %)))) (:errors parent)))
                       (math/mean (map #(* (math/sign %) (math/log (+ E (math/abs %)))) (:errors ind)))))
   :log-diff-0.01 (fn [ind parent _]
                    (- (math/mean (map #(math/log (+ 0.01 %)) (:errors parent)))
                       (math/mean (map #(math/log (+ 0.01 %)) (:errors ind)))))
   :log-abs (fn [ind _ _]
              (- (math/mean (map #(math/log (inc %)) (:errors ind)))))
   :prob-weighted-log (fn [ind _ dist]
                        (selection/prob-weighted-log (:errors ind) dist))
   :parent-prob-weighted-log (fn [ind parent dist]
                               (* (- (math/mean (map #(math/log (inc %)) (:errors parent)))
                                     (math/mean (map #(math/log (inc %)) (:errors ind))))
                                  (let [{probs :probabilities groups :groups} dist
                                        error (:errors parent)
                                        idx (first (keep-indexed #(when (= %2 error) %1) (keys groups)))]
                                    (get probs idx 0))))
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



(defn evaluate-selector
  [bandits dist parents argmap parameter-override]
  (let [{mapper :mapper error-function :error-function
         instructions :instructions max-initial-plushy-size :max-initial-plushy-size
         [forward _] :transformations reward-transformation :reward-transformation} argmap
        initializer (fn [& _] {:plushy (genome/make-random-plushy instructions max-initial-plushy-size)})
        new-ind (fn [bandit-ensemble] #(new-individual %
                                                       argmap
                                                       (or (if (fn? parameter-override) (parameter-override) parameter-override)
                                                           (sample-bandit-ensemble bandit-ensemble :transformation forward))
                                                       initializer
                                                       :preselected? true))
        add-pop (mapper (new-ind bandits) parents)
        add-pop (mapper (partial error-function argmap (:training-data argmap)) add-pop)
        reward-transformation (get reward-transformations reward-transformation)
        rewards (mapper (fn [ind parent] (reward-transformation ind parent dist)) add-pop parents)]
    (pprint/pprint {:rewards (apply max rewards)})
    rewards))

(defn step-generation
  [population bandits argmap & {:keys [dist]}]
  (let [{mapper :mapper error-function :error-function
         parameter-override :parameter-override update-freq :update-freq
         instructions :instructions max-initial-plushy-size :max-initial-plushy-size
         [n population-size] :population-size [forward backward] :transformations
         parameter-analysis :parameter-analysis reward-transformation :reward-transformation} argmap
        ;; _ (println "selection time")
        update-freq (if (or (:samr argmap) parameter-override) population-size update-freq)
        ;; _ (pprint/pprint {:preparent-errs (apply min (map :total-error population))})
        {indices :indices groups :groups :as selection-distribution} (identity (selection/lexicase-distribution population population-size :mapper mapper))
        dist (or dist selection-distribution)
        error-classes (keys groups)
        parents (map #(rand-nth (get groups (nth error-classes %))) indices)
        selected-parents (mapcat #(get groups (nth error-classes %)) (set indices))
        ;; _ (pprint/pprint {:parent-errs (apply min (map :total-error parents))})
        initializer (fn [& _] {:plushy (genome/make-random-plushy instructions max-initial-plushy-size)})
        new-ind (fn [bandit-ensemble] #(new-individual %
                                                       argmap
                                                       (or (if (fn? parameter-override) (parameter-override) parameter-override)
                                                           (:samr argmap)
                                                           (sample-bandit-ensemble bandit-ensemble :transformation forward))
                                                       initializer
                                                       :preselected? true
                                                       :samr? (:samr argmap)))
        parameter-rewards (into {} (mapv #(vector % (evaluate-selector bandits dist parents argmap %)) parameter-analysis))
        parents (partition update-freq update-freq [] parents)]
    (loop [parents parents
           bandits bandits
           new-pop []
           total-rewards []]
      (if (empty? parents)
        (let [pop (into []
                        (take n)
                        (concat new-pop
                                selected-parents))]
          #_(pprint/pprint {:rates (mapv #(:e (:bandit-parameters %)) new-pop)})
          {:evaluated-pop  pop
           :rates (mapv #(:e (:bandit-parameters %)) new-pop)
           :bandits bandits
           :new-pop new-pop
           :report-dist (assoc (dissoc selection-distribution :cases :indices)
                               :rewards (into [] total-rewards)
                               :parameters (mapv #(:e (:bandit-parameters %)) new-pop)
                               :parameter-rewards parameter-rewards
                               :groups (into {} (mapv (fn [[k v]] [k (count v)]) groups)))
           :dist selection-distribution})
        (let [;;_ (println "creation time")
              add-pop (identity (mapper (new-ind bandits) (first parents)))
              ;; _ (println "evaluation time")
              identical (mapv #(= (:plushy %1) (:plushy %2)) add-pop (first parents))
              add-pop (identity (mapper
                                 (fn [ind id? parent]
                                   (if id?
                                     (assoc parent :bandit-parameters (:bandit-parameters ind))
                                     (error-function argmap (:training-data argmap) ind)))
                                 add-pop
                                 identical
                                 (first parents)))
              ;; _ (pprint/pprint {:errs (apply min (mapv :total-error add-pop))})
              ;; _ (println "reward time")
              transform (get reward-transformations reward-transformation)
              rewards (identity (mapper #(transform %1 %2 dist)
                                        add-pop
                                        (first parents)))
              ;; _ (println "update time")
              bandits (identity (if (or (:samr argmap) parameter-override)
                                  bandits
                                  (update-bandit-ensemble (mapv :bandit-parameters add-pop)
                                                          rewards
                                                          bandits
                                                          :mapper mapper
                                                          :transformation backward
                                                          :parallel true)))]
          (recur (rest parents)
                 bandits
                 (concat new-pop add-pop)
                 (concat total-rewards rewards)))))))


(defn finish-gp
  [generation best-ind bandits success argmap]
  (let [{error-function :error-function} argmap]
    (gp/report-success generation best-ind (:error-function argmap) argmap)
    (print-bandits bandits)
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

(defn update-info
  ([info dist generation & {:keys [evaluations]}]
   (let [{cur-dist :dist
          gen-per-update :generations-per-update
          dt :dt
          prev-t :t
          prev-evals :evaluations} info
         cur-t (java.lang.System/currentTimeMillis)
         dist (if (zero? (mod generation gen-per-update)) dist cur-dist)]
     (assoc info
            :t cur-t
            :dist dist
            :dt (conj dt (- cur-t prev-t))
            :evaluations (if evaluations (+ evaluations prev-evals) prev-evals))))
  ([generations-per-update]
   {:dist nil
    :generations-per-update generations-per-update
    :t (java.lang.System/currentTimeMillis)
    :dt []
    :evaluations 0}))


(defn make-default
  [argmap]
  (merge {:solution-error-threshold 0
          :restart 0
          :mapper concurrent/service-mapper
          :transformations [tile-to-parameters-0 parameters-to-tile-0]
          :error-transformation (get-error-transformation :identity :array-errors false)
          :update-freq 100
          :report-interval 1
          :reward-transformation :selection-prob-diff
          :generations-per-update 1
          :initializer (fn [_] {:plushy (genome/make-random-plushy (:instructions argmap) (:max-initial-plushy-size argmap))})}
         (update argmap
                 :population-size
                 #(if (number? %) [% %] %))))

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
           transformations [tile-to-parameters-0 parameters-to-tile-0]
           mapper concurrent/service-mapper
           generations-per-update 1
           error-transformation (get-error-transformation :identity :array-errors false)}
    :as   argmap}]
  (let [argmap (make-default argmap)
        _ (do (gp/print-argmap {:starting-args argmap})
              (println))
        {population-size :population-size
         report-interval :report-interval
         gens-per-update :generations-per-update
         initializer :initializer} argmap
        init-pop (mapper
                  initializer
                  (range (second population-size)))
        init-pop (if (:samr argmap) (mapv #(assoc %1 :bandit-parameters {:e (math/exp (- (* 10 (/ %2 (count init-pop))) 10))}) init-pop (range (count init-pop))) init-pop)
        bandits (if (or (:samr argmap) parameter-override) nil
                    (mapv #(evaluate-bandit % :mapper mapper)
                          (apply bandit-ensemble
                                 (into [] (mapcat identity
                                                  bandit-parameters)))))]
    (loop [generation 0
           population (identity (mapper
                                 (comp error-transformation
                                       (partial error-function argmap (:training-data argmap)))
                                 init-pop))
           bandits bandits
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
            (finish-gp generation best-ind bandits true argmap)
            (let [{new-pop :new-pop evaluated-pop :evaluated-pop bandits :bandits dist :dist report-dist :report-dist rates :rates} (step-generation population bandits argmap :dist (:dist into))]
              (when (zero? (mod generation report-interval))
                (custom-report evaluated-pop generation argmap :new-pop new-pop :t (- (java.lang.System/currentTimeMillis) (:t info)) :bandits bandits :dist report-dist :rates rates))
              (recur (inc generation)
                     evaluated-pop
                     (mapv #(assoc % :generation (inc generation)) bandits)
                     (update-info info dist generation))))
          ;;
          (>= generation max-generations) (finish-gp generation best-individual bandits false argmap)
          ;;
          :else (let [{new-pop :new-pop evaluated-pop :evaluated-pop bandits :bandits dist :dist report-dist :report-dist rates :rates} (step-generation population bandits argmap :dist (:dist info))]
                  (when (zero? (mod generation report-interval))
                    (custom-report evaluated-pop generation argmap :new-pop new-pop :t (- (java.lang.System/currentTimeMillis) (:t info)) :bandits bandits :dist report-dist :rates rates))
                  (recur (inc generation)
                         evaluated-pop
                         (mapv #(assoc % :generation (inc generation)) bandits)
                         (update-info info dist generation))))))))

(defn create-generation
  [{:keys [mapper population-size instructions max-initial-plushy-size
           population-size report-interval gens-per-update downsample? ds-parent-rate
           ds-parent-gens ids-type mapper error-function solution-error-threshold dont-end]
    :as argmap}]
  (let [population (mapper
                    (fn [_] {:plushy (genome/make-random-plushy instructions max-initial-plushy-size)})
                    (range (second population-size)))
        indexed-training-data (if downsample?
                                (downsample/assign-indices-to-data (downsample/initialize-case-distances argmap) argmap)
                                (:training-data argmap))
        training-data (if downsample?
                        (case (:ds-function argmap)
                          :case-maxmin (downsample/select-downsample-maxmin indexed-training-data argmap)
                          :case-maxmin-auto (downsample/select-downsample-maxmin-adaptive indexed-training-data argmap)
                          :case-rand (downsample/select-downsample-random indexed-training-data argmap)
                          (do (pprint/pprint {:error "Invalid Downsample Function"})
                              (downsample/select-downsample-random indexed-training-data argmap)))
                        indexed-training-data)
        parent-reps (if
                     downsample?
                      (take (* ds-parent-rate (count population)) (shuffle population))
                      '())
        rep-evaluated-pop (if downsample?
                            (sort-by :total-error
                                     (mapper
                                      (partial error-function argmap indexed-training-data)
                                      parent-reps))
                            '())
        evaluated-pop (sort-by :total-error
                               (mapper
                                (partial error-function argmap training-data)
                                population))
        evaluations (+ 0
                       (* (if (number? population-size) population-size (second population-size)) (count training-data)) ;every member evaluated on the current sample
                       (* (count parent-reps)
                          (- (count indexed-training-data)
                             (count training-data))))
        indexed-training-data (if downsample?
                                (downsample/update-case-distances rep-evaluated-pop
                                                                  indexed-training-data
                                                                  indexed-training-data
                                                                  ids-type
                                                                  (/ solution-error-threshold
                                                                     (count indexed-training-data)))
                                indexed-training-data)]
    {:population evaluated-pop
     :indexed-training-data indexed-training-data
     :evaluations evaluations}))



(defn downsample-gp
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
           transformations error-transformation generations-per-update]
    :or   {solution-error-threshold 0.0
           transformations [tile-to-parameters-0 parameters-to-tile-0]
           mapper concurrent/service-mapper
           generations-per-update 1
           error-transformation (get-error-transformation :nlog :array-errors false)}
    :as   argmap}]
  (let [argmap (make-default argmap)
        _ (do (gp/print-argmap {:starting-args argmap})
              (println))
        {:keys [population-size report-interval generations-per-update downsample? ds-parent-rate
                ds-parent-gens ids-type mapper error-function solution-error-threshold dont-end]} argmap
        {:keys [population evaluations indexed-training-data]} (create-generation argmap)
        bandits (if parameter-override nil
                    (mapper evaluate-bandit
                            (apply bandit-ensemble
                                   (into [] (mapcat identity
                                                    bandit-parameters)))))]
    (loop [generation 0
           population population
           indexed-training-data (if downsample?
                                   (downsample/assign-indices-to-data (downsample/initialize-case-distances argmap) argmap)
                                   (:training-data argmap))
           bandits bandits
           info (assoc (update-info generations-per-update) :evaluations evaluations)]
      (let [parent-reps (if
                         (and downsample? ; if we are down-sampling
                              (zero? (mod generation ds-parent-gens))) ;every ds-parent-gens generations
                          (take (* ds-parent-rate (count population)) (shuffle population))
                          '())
            rep-evaluated-pop (if downsample?
                                (sort-by :total-error
                                         (mapper
                                          (partial error-function argmap indexed-training-data)
                                          parent-reps))
                                '())
            population (if (#{:plexicase :epsilon-plexicase} (:parent-selection argmap)) (selection/assign-probabilities population argmap) population)
            best-individual (apply min-key :total-error population)
            best-individual-passes-ds (and downsample? (<= (:total-error best-individual) solution-error-threshold))
            argmap (condp = (:parent-selection argmap)
                     :epsilon-lexicase (assoc argmap :epsilons (selection/epsilon-list population))
                     :plexicase (selection/reduction-probs population argmap)
                     :epsilon-plexicase (selection/reduction-probs population argmap)
                     argmap)
            training-data (if downsample?
                            (case (:ds-function argmap)
                              :case-maxmin (downsample/select-downsample-maxmin indexed-training-data argmap)
                              :case-maxmin-auto (downsample/select-downsample-maxmin-adaptive indexed-training-data argmap)
                              :case-rand (downsample/select-downsample-random indexed-training-data argmap)
                              (do (pprint/pprint {:error "Invalid Downsample Function"})
                                  (downsample/select-downsample-random indexed-training-data argmap)))
                            indexed-training-data)
            argmap (assoc argmap :training-data training-data)]
        (when best-individual-passes-ds
          (prn {:semi-success-generation generation}))
        (cond
          ;; Success on training cases is verified on testing cases
          (or (and best-individual-passes-ds
                   (<= (:total-error (error-function argmap indexed-training-data best-individual))
                       solution-error-threshold))
              (and (not downsample?)
                   (<= (:total-error best-individual)
                       solution-error-threshold)))
          (do (when (:simplification? argmap)
                (let [simplified-plushy (simplification/auto-simplify-plushy (:plushy best-individual) error-function argmap)]
                  (prn {:total-test-error-simplified
                        (:total-error (error-function argmap (:testing-data argmap) {:plushy simplified-plushy}))})
                  (prn {:simplified-plushy simplified-plushy})
                  (prn {:simplified-program (genome/plushy->push simplified-plushy argmap)})))
              (finish-gp generation best-individual bandits true argmap))
          ;;
          (or (and (not downsample?)
                   (>= generation max-generations))
              (and downsample?
                   (>= evaluations (* max-generations (if (number? population-size) population-size (second population-size)) (count indexed-training-data)))))
          (finish-gp generation best-individual bandits false argmap)
          ;;
          :else (let [{new-pop :new-pop evaluated-pop :evaluated-pop bandits :bandits dist :dist report-dist :report-dist} (step-generation population bandits (assoc argmap :training-data training-data) :dist (:dist info))]
                  (when (zero? (mod generation report-interval))
                    (custom-report evaluated-pop generation argmap :new-pop new-pop :t (- (java.lang.System/currentTimeMillis) (:t info)) :bandits bandits :dist report-dist :succinct (not (zero? (mod generation ds-parent-gens)))))
                  (recur (inc generation)
                         evaluated-pop
                         (if downsample?
                           (if (zero? (mod generation ds-parent-gens))
                                                  ; update distances every ds-parent-gens generations
                             (downsample/update-case-distances rep-evaluated-pop
                                                               indexed-training-data
                                                               indexed-training-data
                                                               ids-type
                                                               (/ solution-error-threshold
                                                                  (count indexed-training-data)))
                             indexed-training-data)
                           indexed-training-data)
                         (mapv #(assoc % :generation (inc generation)) bandits)
                         (update-info info dist generation :evaluations (+ (* (if (number? population-size) population-size (second population-size)) (count training-data)) ;every member evaluated on the current sample
                                                                           (if (zero? (mod generation ds-parent-gens))
                                                                             (* (count parent-reps)
                                                                                (- (count indexed-training-data)
                                                                                   (count training-data)))
                                                                             0)
                                                                           (if best-individual-passes-ds
                                                                             (- (count indexed-training-data) (count training-data))
                                                                             0))))))))))



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

(defn process-file
  [file out-file & {:keys [key filter-key] :as kek}]
  (let [{key :key filter-key :filter-key} (apply hash-map (map #(if (string? %) (read-string %) %) (mapcat identity kek)))
        out-file (if (= (string/lower-case out-file) "none") nil out-file)
        f (if (string? file) (read-string (str "[" (slurp file) "]")) file)
        f (if filter-key (filter filter-key f) f)
        f (cons (first f) (take-while #(not (= (:generation %) 0)) (rest f)))
        f (if key (mapv key (filter key f)) f)]
    (if (nil? out-file)
      (pprint/pprint f)
      (spit out-file (with-out-str (println (jsonify f)))))))

(defn get-processor
  [iter & names]
  (doall
   (for [name names]
     (dotimes [i (read-string iter)]
       (doall
        (for [x [".weights :key :m"  ".freq :key :N" ".args :key :starting-args"]]
          (println (str "lein run -m propeller.tools.maxbandit/process-file maxbandit-results/" name "/" (inc i) ".out maxbandit-results/" name "/" (inc i) x " :filter-key :generation"))))))))

(defn enact-processor
  [iter & names]
  (doall
   (for [name names]
     (do (println name)
         (dotimes [i (read-string iter)]
           (let [dir  "umad-results/" #_"maxbandit-results-fixlexicase/"
                 f (slurp (str dir name "/" (inc i) ".out"))
                ;;  f (string/join "\n" (drop 169 (string/split-lines f)))
                 f (str "[" f "]")
                 f (read-string f)]
            ;;  (println (first ))
             (process-file f (str dir name "/" (inc i) ".args") :key :starting-args)
             (let [f (filter :generation f)]
               (pprint/pprint {:keys (mapv #(keys (:parameter-rewards (:dist %))) f)})
               (doall
                (for [[x y] [[".weights" #(mapv (fn [b] (:w (:base-coding b))) (:bandits %))]
                             [".freq" #(mapv (fn [b] (:N (:base-coding b))) (:bandits %))]
                             [".errs" :best-errors]
                             [".param_analysis" #(:parameter-rewards (:dist %))]
                             [".groups" #(:groups (:dist %))]
                             [".rewards" #(mapv float (:rewards (:dist %)))]
                             [".rates" #(mapv float (:rates %))]
                             [".probs" #(:probabilities (:dist %))]]]
                  (process-file f (str dir name "/" (inc i) x) :key y))))))))))



;;PSB Checklist:
;; - Checksum 1 5 0 4 2 1 = >17h x 10
;; - Compare String Lengths 32 42 26 22 28 5 (100 cases, 200/600) = 1.5d
;; - Count Odds 8 12 6 10 7 5 = 10h x 10
;; - Digits 19 11 10 15 14 10 (100 cases, 150/600) = 4h x 10
;; - Double Letters 19 20 5 18 16 1 (100 cases) = 10h x 10
;; - Even Squares 0 0 0 0 0 0 (200 cases, 200/2000)
;; - For Loop Index 2 1 0 1 1 0 (100 cases, 150/600)
;; - Grade 0 0 0 0 0 1 = 14h x 10
;; - Last Index of Zero 62 56 62 49 72 29 (200 cases, 150/600)
;; - Median 55 48 66 60 55 54 = 4h
;; - Mirror Image 100 100 100 100 100 87 (100 cases, 150/600) = 1h x 10
;; - Negative To Zero 80 82 73 72 79 62 = 18h x 10
;; - Number IO 98 100 100 100 100 100 = 10min
;; - Pig Latin 0 0 0 0 0 0
;; - Replace Space With Newline 87 87 88 83 92 58 (100 cases, 400/1600)
;; - Scrabble Score 13 20 3 11 10 4 = 1.5h x 10
;; - Small or Large 7 4 9 4 6 5 (100 cases, 100/300) = 12h
;; - Smallest 100 100 98 99 86 97 (100 cases, 100/200) = 19min
;; - String Lengths Backwards 94 86 80 82 62 74 (100 cases, 150/600) = 2.5d
;; - Sum of Squares 21 26 13 10 13 3  = 10h x10
;; - Super Anagrams 4 0 2 3 1 0
;; - Syllables 38 48 21 51 28 24 (100 cases, 400/1600)
;; - Vector Average 88 92 84 77 82 43 = 4d?
;; - Vectors Summed 11 9 3 9 7 1
;; - X-Word Lines 61 59 12 63 25 18
;;PSB2 Checklist:
;; - Basement 10h x 10
;; - Fizz Buzz 13h x 10
;; - Bouncing Balls 16h x 10?
;; - GCD 14h x 10
;;Regression:
;; - uball5d = 1.5d
;; - korns = 4h 
;; - pagie = 1.5d

