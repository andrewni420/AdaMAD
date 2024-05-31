(ns propeller.tools.math
  "Math functions.")

(defonce ^{:no-doc true :const true} PI #?(:clj  Math/PI
               :cljs js/Math.PI))

(defonce ^{:no-doc true :const true} E #?(:clj  Math/E
              :cljs js/Math.PI))

(defn mean
  "Returns the mean."
  [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum (float count))
      0.0)))

(defn std 
  [coll]
  (let [m (mean coll)]
    (Math/sqrt (mean (map #(Math/pow (- % m) 2) coll)))))

(defn median
  "Returns the median."
  [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2.0)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))


(defn dot
  "Dot product"
  [& colls]
  (reduce + (apply map * colls)))

(defn generalized-dot
  [coll1 coll2 & {:keys [pow]
                  :or {pow 1}}]
  (assert (not (zero? pow)) "Generalized mean power cannot be 0")
  (Math/pow (/ (reduce + (map (comp #(Math/pow % pow) *) coll1 coll2)) (min (count coll2) (count coll1))) (/ 1 pow)))


(defn adot
  "Array dot product"
  [& colls]
  (areduce ^doubles (first colls) idx ret 0. (+ ret (apply * (map #(aget ^doubles % idx) colls)))))

(defn aplus
  "Array addition"
  [& colls]
  (amap ^doubles (first colls) idx _ (apply + (map #(aget ^doubles % idx) colls))))


(defn median-absolute-deviation
  "Returns the median absolute deviation."
  [coll]
  (let [median-val (median coll)]
    (median (map #(Math/abs (- % median-val)) coll))))


(defn abs
  "Returns the absolute value of a number."
  [x]
  (if (neg? x) (- x) x))

(defn approx=
  "Returns true if the absolute difference between x and y is less than or
  equal to some specified error level, epsilon."
  [x y epsilon]
  (<= (abs (- y x)) epsilon))

(defn ceil
  "Returns the smallest integer greater than or equal to x."
  [x]
  #?(:clj  (Math/ceil x)
     :cljs (js/Math.ceil x)))

(defn cos
  "Returns the cosine of an angle (specified in radians)."
  [x]
  #?(:clj  (Math/cos x)
     :cljs (js/Math.cos x)))

(defn div
  "Returns the result of floating point division between x and y."
  [x y]
  (double (/ x y)))

(defn exp
  "Returns Euler's number (approx. 2.71) raised to the given power."
  [x]
  #?(:clj  (Math/exp x)
     :cljs (js/Math.exp x)))

(defn floor
  "Returns the largest integer less than or equal to x."
  [x]
  #?(:clj  (Math/floor x)
     :cljs (js/Math.floor x)))

(defn log
  "Returns the logarithm of x with the given base. If called with only one
  argument, returns the natural logarithm (base e) of the given value."
  ([x base]
   (/ (log x) (log base)))
  ([x]
   #?(:clj  (Math/log x)
      :cljs (js/Math.log x))))

(defn pow
  "Returns the value obtained by raising the first argument to the power of
  the second argument."
  [x n]
  #?(:clj  (Math/pow x n)
     :cljs (js/Math.pow x n)))

(defn root
  "Returns the root of x with base n."
  [x n]
  (pow x (/ 1 n)))

(defn round
  "Returns the value of x rounded to the nearest integer."
  [x]
  #?(:clj  (Math/round x)
     :cljs (js/Math.round x)))


(defn bigdec-round
  [x scale]
  (.setScale x scale java.math.BigDecimal/ROUND_HALF_EVEN))

(defn bigdec-exp
  ([x n & {:keys [scale]
           :or {scale 10}}]
   (let [r (Math/floor n)
         q (- n r)]
     (bigdec-round
      (* (.pow (bigdec x) r)
         (bigdec (Math/pow x q)))
      scale)))
  ([n] (bigdec-exp Math/E n)))

(defn big*
  [x y & {:keys [scale]}]
  (bigdec-round (* (bigdec x) (bigdec y)) 10))

(defn sign
  "Returns the 1 if the argument is positive, -1 if the argument is negative,
  and 0 if the argument is zero."
  [x]
  (cond (< x 0) -1
        (> x 0) 1
        :else 0))

(defn sin
  "Returns the sine of an angle (specified in radians)."
  [x]
  #?(:clj  (Math/sin x)
     :cljs (js/Math.sin x)))

(defn sqrt
  "Returns the square root of the given value."
  [x]
  #?(:clj  (Math/sqrt x)
     :cljs (js/Math.sqrt x)))

(defn square
  "Returns the square of the given value."
  [x]
  (* x x))

(defn tan
  "Returns the tangent of an angle (specified in radians)."
  [x]
  #?(:clj  (Math/tan x)
     :cljs (js/Math.tan x)))

(defn softmax
  [coll & {:keys [temperature]
           :or {temperature 1}}]
  (let [coll (mapv #(/ % temperature) coll)
        s (mapv #(Math/exp %) coll)
        sum (reduce + s)]
    (if (zero? sum)
      (mapv (constantly 0) s)
      (mapv #(/ % sum) s))))

(defn asoftmax
  [^doubles coll & {:keys [temperature]
           :or {temperature 1}}]
  (let [^doubles s (amap coll idx ret (Math/exp (/ (aget coll idx) temperature)))
        sum (areduce s idx ret 0. (+ ret (aget s idx)))]
    (if (zero? sum)
      (amap s idx ret 0.)
      (amap s idx ret (/ (aget s idx) sum)))))

(defn transpose
  "returns a vector containing the transpose of a coll of colls"
  [x]
  (apply map vector x))

(defn step
  "returns 1 if number is nonzero, 0 otherwise"
  [x]
  (if (zero? x) 0 1))