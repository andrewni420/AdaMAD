(ns propeller.tools.distributions
  "Functions to calculate distribution."
  (:require [propeller.tools.calculus :as calculus]
            [propeller.tools.math :as math]
            [clojure.pprint :as pprint])
  (:import java.util.Random))

;; =============================================================================
;; NORMAL
;; =============================================================================
(def generator 
  "Java's random number generator"
  (Random.))

(defn rand-normal-java 
  "Generate n random normal numbers using Java's generator"
  [n mu sigma]
  (repeatedly n #(+ mu (* sigma (.nextGaussian generator)))))

(defn- box-muller
  "Given two uniformly distributed random variables (from 0 to 1), returns a
  Standard Normal variable computed using the Box-Muller Transform."
  [u1 u2]
  (* (math/sqrt (* -2 (math/log u1)))
     (math/cos (* 2 math/PI u2))))

(defn- normal-pdf
  "Given a mean and standard deviation, returns the corresponding Normal
  Probability Distribution Function."
  [mu sigma]
  (fn [x]
    (* (/ 1 (* sigma (math/sqrt (* 2 math/PI))))
       (math/exp (- (/ (math/pow (/ (- x mu) sigma) 2) 2))))))

(defn rand-norm
  "Generates n Normally-distributed random variables with given mean and
  standard deviation. If no parameters are provided, defaults to a
  single random observation from a Standard Normal distribution.
  Accepts an argument map with optional keys :n, :mu, and :sigma."
  [{:keys [n mu sigma]
    :or   {n 1, mu 0, sigma 1}}]
  (repeatedly n #(+ mu (* sigma (box-muller (rand) (rand))))))


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

(defn get-distribution
  [distribution n scale offset]
  (condp = distribution
    :range (map (comp (partial + offset)
                      (partial * (/ scale n) (Math/sqrt 12))) 
                (shuffle (range n)))
    :uniform (repeatedly n #(+ offset (* scale (rand) (Math/sqrt 12))))
    :normal (into [] (eduction
                      (mapcat identity)
                      (take n)
                      (repeatedly #(rand-normal offset scale :pair? true))))))

(defn pdf-norm
  "Returns the value of the Normal Probability Distribution Function at a
  particular value x. If no distributional parameters are provided, defaults to
  the Standard Normal PDF.
  Accepts an argument map with keys :x, and optionally :mu and :sigma."
  [{:keys [x mu sigma]
    :or   {mu 0, sigma 1}}]
  ((normal-pdf mu sigma) x))

(defn cdf-norm
  "Parameters: {:keys [x mu sigma]}
  Returns the value of the Normal Cumulative Distribution Function at a
  particular value x. If no distributional parameters are provided, defaults to
  the Standard Normal CDF.
  Accepts an argument map with keys :x, and optionally :mu and :sigma."
  [{:keys [x mu sigma]
    :or   {mu 0, sigma 1}}]
  (calculus/integrate (normal-pdf mu sigma) (- mu (* 6 sigma)) x))

(defn quant-norm
  "For a given probability p, returns the corresponding value of the quantile
  function (i.e. the inverse Cumulative Distribution Function). If no
  distributional parameters are provided, defaults to Standard Normal quantiles.
  Accepts an argument map with keys :p, and optionally :mu and :sigma."
  [{:keys [p mu sigma]
    :or   {mu 0, sigma 1}}]
  ())                                                       ; unfinished...


(defn btrd-f
  [k]
  (case k
    0 0.08106146679532726
    1 0.04134069595540929
    2 0.02767792568499834
    3 0.02079067210376509
    4 0.01664469118982119
    5 0.01387612882307075
    6 0.01189670994589177
    7 0.01041126526197209
    8 0.009255462182712733
    9 0.008330563433362871
    (let [k' (inc k) k2' (math/square k')]
      (double (/ (- 0.08333333333333333
                    (/ (- 0.002777777777777778
                          (/ 7.936507936507937E-4 k2')) k2')) k')))))

(defn rand-binomial-btrd
  "Algorithm BTRD from \"The Generation of Binomial Random Variates\", Wolfgang Hormann, p6"
  [n p]
  (if (> p 0.5)
    (- n (rand-binomial-btrd n (- 1 p)))
    (let [m (int (* (inc n) p))
          q (- 1 p)
          r (/ p q)
          nr (* (inc n) r)
          npq (* n p q)
          rnpq (math/sqrt npq)
          b (+ 1.15 (* 2.53 rnpq))
          a (+ -0.0873 (* 0.0248 b) (* 0.01 p))
          c (+ (* n p) 0.5)
          alpha (* (+ 2.83 (/ 5.1 b)) rnpq)
          vr (- 0.92 (/ 4.2 b))
          urvr (* 0.86 vr)]
      ;; 1
      (loop []
        (let [v (rand)]
          (if (<= v urvr)
            (let [u (- (/ v vr) 0.43)]
              (int (+ (* (+ (/ (* 2 a) (- 0.5 (abs u))) b) u) c)))
            (let [;; 2
                  [u v] (if (>= v vr)
                          [(- (rand) 0.5) v]
                          (let [u (- (/ v vr) 0.93)]
                            [(- (* 0.5 (if (pos? u) 1 -1)) u) (* (rand) vr)]))
                  ;; 3
                  us (- 0.5 (abs u))
                  k (int (+ (* (+ (* 2 (/ a us)) b) u) c))]
              (if (<= 0 k n)
                (let [v (* v (/ alpha (+ (/ a (math/square us)) b)))
                      km (abs (- k m))]
                  (if (<= km 15)
                    ;; 3.1
                    (let [f 1.0
                          fx (fn [x i] (* x (- (/ nr (inc i)) r)))
                          [f v] (if (< m k)
                                  [(reduce fx f (range m k)) v]
                                  [f (reduce fx v (range k m))])]
                      (if (<= v f) k (recur)))
                    ;; 3.2
                    (let [v (math/log v)
                          p (* (/ km npq) (+ (/ (+ (* (+ (/ km 3) 0.625) km) 0.1666666666666667) npq) 0.5))
                          t (/ (* (- km) km) (* 2 npq))]
                      (cond
                        (< v (- t p)) k
                        (> v (+ t p)) (recur)
                        :else
                        ;; 3.3
                        (let [nm (inc (- n m))
                              h (+ (* (+ m 0.5) (math/log (/ (inc m) (* r nm)))) (btrd-f m) (btrd-f (- n m)))
                              ;; 3.4
                              nk (inc (- n k))]
                          (if (<= v (+ h
                                       (* (inc n) (math/log (/ nm nk)))
                                       (* (+ k 0.5) (math/log (/ (* nk r) (inc k))))
                                       (- (btrd-f k))
                                       (- (btrd-f (- n k)))))
                            k
                            (recur)))))))
                (recur )))))))))

(defn rand-binomial-binv
  [n p]
  (if (> p 0.5)
    (- n (rand-binomial-binv n (- 1 p)))
    (let [cutoff 110
          q (- 1 p)
          s (/ p q)]
      (loop [ix 0 f (math/pow q n) u (rand)]
        (cond
          (< u f) ix
          (>= ix cutoff) (rand-binomial-binv n p)
          :else (recur (inc ix) (* f s (/ (- n ix) (inc ix))) (- u f)))))))

(defn rand-binomial
  [n p]
  (let [p (max 0.0 (min p 1.0))]
    (cond
      (= p 0.0) 0
      (= p 1.0) n
      (< (* n p) 14) (rand-binomial-binv n p)
      :else (rand-binomial-btrd n p))))