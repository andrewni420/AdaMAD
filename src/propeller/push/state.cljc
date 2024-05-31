(ns propeller.push.state
  "Push states"
  (:require [propeller.push.limits :as l]
            #?(:cljs [goog.string :as gstring]))
  (:import clojure.lang.APersistentMap
           clojure.lang.PersistentArrayMap))


;; Empty push state - all available stacks are empty
(defonce ^:no-doc empty-state {:boolean        '()
                      :char           '()
                      :code           '()
                      :exec           '()
                      :float          '()
                      :input          {}
                      :output         {}
                      :integer        '()
                      :print          '("")
                      :string         '()
                      :vector_boolean '()
                      :vector_float   '()
                      :vector_integer '()
                      :vector_string  '()})


;; All stack types available in a Push state
(defonce ^:no-doc stacks (set (keys empty-state)))

;; All vector stack types available in a Push state, with their corresponding
;; element types
(defonce ^:no-doc vec-stacks {:vector_boolean :boolean
                     :vector_float   :float
                     :vector_integer :integer
                     :vector_string  :string})

(defonce ^:no-doc stack-limiter {:exec           l/limit-code
                        :code           l/limit-code
                        :integer        #(long (l/limit-number %))
                        :float          l/limit-number
                        :string         l/limit-string
                        :vector_boolean l/limit-string
                        :vector_float   #(mapv l/limit-number (l/limit-vector %))
                        :vector_integer #(mapv (fn [i] (int (l/limit-number i))) (l/limit-vector %))
                        :vector_string  #(mapv (fn [s] (l/limit-string s)) (l/limit-vector %))})

(def example-state "Example of a Push state." {:exec    '()
                    :integer '(1 2 3 4 5 6 7)
                    :string  '("abc")
                    :input   {:in1 4}})

;; Returns true if the stack is empty
(defn empty-stack?
  "Returns true if the stack is empty"
  [^APersistentMap state stack]
  (empty? (.get state stack)))

;; Returns the stack size
(defn stack-size
  "Returns the stack size"
  [state stack]
  (count (get state stack)))

(def peek-stack-counter (atom 0))

;; Returns the top item on the stack
(defn peek-stack
  "Returns the top item on the stack"
  [^APersistentMap state stack]
  (swap! peek-stack-counter inc)
  (if (empty? (.get state stack))
    :no-stack-item
    (first (.get state stack))))

(def peek-stack-many-counter (atom 0))

;; Returns the top n items on the stack, as a chunk. If there are less than n
;; items on the stack, returns the entire stack
(defn peek-stack-many
  "Returns the top n items on the stack, as a chunk. If there are less than n
  items on the stack, returns the entire stack"
  [state stack n]
  (swap! peek-stack-many-counter inc)
  (take n (get state stack)))

(def pop-stack-counter (atom 0))

;; Removes the top item of stack
(defn pop-stack
  "Removes the top item of stack"
  [^APersistentMap state stack]
  (swap! pop-stack-counter inc)
  (.assoc state stack (rest (.get state stack))))

#_(let [a {:hi [1 2 3 4 5 6 7]}]
  (time (dotimes [_ 400000]
          (pop-stack a :hi))))

(def pop-stack-many-counter (atom 0))
;; Pops the top n items of the stack. If there are less than n items on the
;; stack, pops the entire stack
(defn pop-stack-many
  "Pops the top n items of the stack. If there are less than n items on the
  stack, pops the entire stack"
  [state stack n]
  (swap! pop-stack-many-counter inc)
  (update state stack (partial drop n)))

(def push-stack-counter (atom 0))

;; Pushes an item onto the stack
(defn push-to-stack
  "Pushes an item onto the stack"
  [state stack item]
  (swap! push-stack-counter inc)
  (if (or (nil? item)
          (>= (stack-size state stack) l/max-stack-items))
    state
    (let [limiter (get stack-limiter stack identity)]
      (update state stack conj (limiter item)))))

#_(defn push-to-stack
  "Returns a copy of the state with the value pushed on the named stack. This is a utility,
   not for use in Push programs."
  [state stack item]
  (when-not (nil? item)
   (assoc state stack (cons item (stack state)))))

(def push-stack-many-counter (atom 0))

@push-stack-many-counter ;61000
@push-stack-counter ;64199
@pop-stack-counter ;573400
@pop-stack-many-counter ;1200
@peek-stack-counter ;185400 
@peek-stack-many-counter ;1200

;; Pushes a collection of items onto the stack, as a chunk (i.e. leaving them in
;; the order they are in)
(defn push-to-stack-many
  "Pushes a collection of items onto the stack, as a chunk (i.e. leaving them in
   the order they are in)"
  [state stack items]
  (swap! push-stack-many-counter inc)
  (let [items (if (coll? items) items (list items))
        items-no-nil (filter #(not (nil? %)) items)
        items-to-push (take (- l/max-stack-items (stack-size state stack)) items-no-nil)
        limit (get stack-limiter stack identity)]
    (update state stack into (map limit (reverse items-to-push)))))

;; Takes a state and a collection of stacks to take args from. If there are
;; enough args on each of the desired stacks, returns a map with keys
;; {:state :args}, where :state is the new state and :args is a list of args
;; popped from the stacks. If there aren't enough args on the stacks, returns
;; :not-enough-args without popping anything
(defn get-args-from-stacks
  "Takes a state and a collection of stacks to take args from. If there are
   enough args on each of the desired stacks, returns a map with keys
   {:state :args}, where :state is the new state and :args is a list of args
   popped from the stacks. If there aren't enough args on the stacks, returns
   :not-enough-args without popping anything"
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [current-stack (first stacks)]
        (if (empty-stack? state current-stack)
          :not-enough-args
          (recur (pop-stack state current-stack)
                 (rest stacks)
                 (conj args (peek-stack state current-stack))))))))


;; Pretty-print a Push state, for logging or debugging purposes
(defn print-state
  "Pretty-print a Push state, for logging or debugging purposes"
  [state]
  (doseq [stack (keys empty-state)]
    #?(:clj  (printf "%-15s = " stack)
       :cljs (print (gstring/format "%-15s = " stack)))
    (prn (if (get state stack) (get state stack) '()))
    (flush)))
