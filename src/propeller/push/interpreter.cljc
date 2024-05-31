(ns propeller.push.interpreter
  "Interprets Push programs."
  (:require [propeller.push.instructions :as instructions]
            [propeller.push.state :as state]
            [propeller.push.instructions.input-output :as io]))


(defn interpret-one-step
  "Takes a Push state and executes the next instruction on the exec stack."
  [state]
  (let [popped-state (state/pop-stack state :exec)
        instruction (first (:exec state))
        literal-type (instructions/get-literal-type instruction)]     ; nil for non-literals
    (cond
      ;;
      ;; Recognize functional instruction or input instruction
      (keyword? instruction)
      (if-let [function (instruction @instructions/instruction-table)]
        (function popped-state)
        (io/handle-input-instruction popped-state instruction))
      ;;
      ;; Recognize constant literal instruction
      literal-type
      (if (= :generic-vector literal-type)
        ;; Empty vector gets pushed on all vector stacks
        (reduce #(update-in % [%2] conj []) popped-state
                [:vector_boolean :vector_float :vector_integer :vector_string])
        (state/push-to-stack popped-state literal-type instruction))
      ;;
      ;; Recognize parenthesized group of instructions
      (seq? instruction)
      (update popped-state :exec #(concat %2 %1) instruction)
      ;;
      :else
      (throw #?(:clj  (Exception. (str "Unrecognized Push instruction in program: "
                                       (name instruction)))
                :cljs (js/Error. (str "Unrecognized Push instruction in program: "
                                      (name instruction))))))))

(def push-types '(:exec :code :integer :float :boolean :char :string :zip
                        :vector_integer :vector_float :vector_boolean :vector_string
                        :input :output :auxiliary :tag :return :environment :genome
                        :gtm))

(defn keyword->symbol [kwd]
  "Returns the symbol obtained by removing the : from a keyword."
  (symbol (name kwd)))

(defmacro define-push-state-record-type []
  `(defrecord ~'PushState [~@(map keyword->symbol push-types)]))

(define-push-state-record-type)

(let [empty-state (map->PushState {})]
  (defn make-push-state
    "Returns an empty push state."
    [] empty-state))

(defn push-item
  "Returns a copy of the state with the value pushed on the named stack. This is a utility,
   not for use in Push programs."
  [value -type state]
  (assoc state -type (cons value (-type state))))

#_(let [s state/empty-state
      s (assoc s :integer '(1 2 3 4 5)
               :exec '(1))]
  (time (dotimes [_ 10000]
          (interpret-one-step s))))

#_(let [s state/empty-state
      s (assoc s :integer '(1 2 3 4 5)
               :exec '(1))]
  (time (dotimes [_ 573400]
          (state/peek-stack s :integer))))

#_(let [s (make-push-state)]
  (time (dotimes [_ 10000]
          (push-item 1 :integer s))))

(def step-counter (atom 0))

@step-counter
;0.0725139msecs per step
;0.004msecs per step
;0.0064 msecs per step
;0.00668 msecs per step

(defn interpret-program
  "Runs the given problem starting with the stacks in start-state. If the
  start-state includes the key :keep-history with a truthy value, then
  the returned state will include the key :history with a value that is a
  vector containing all states prior to the final state."
  [program start-state step-limit]
  (loop [state (assoc start-state :exec (list program) :step 1)
         history []]
    (if (or (empty? (:exec state))
            (> (:step state) step-limit))
      (do (swap! step-counter (partial + (:step state)))
        (if (:keep-history state)
        (assoc state :history history)
        state))
      (recur (update (interpret-one-step state) :step inc)
             (when (:keep-history state) (conj history state))))))
