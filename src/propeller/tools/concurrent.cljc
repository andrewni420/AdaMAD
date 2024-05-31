(ns propeller.tools.concurrent
  (:import java.util.concurrent.ExecutorService
           java.util.concurrent.Executors
           java.util.concurrent.Callable
           java.util.concurrent.TimeUnit
           java.lang.Runtime
           java.lang.Thread))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A collection of concurrency utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ExecutorService based concurrency:
;;;
;;; Global work-stealing thread pool with size equal to 
;;; the number of processors handles all asynchronous tasks
;;;
;;; submit and msubmit (macro version) submit a runnable, which is implemented by an ifn with 
;;; no arguments, and returns a future that can be derefed to obtain the result. 
;;; The submitted runnable is added to the global ExecutorService's task queue
;;; and is picked up by threads in the pool when available
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^ExecutorService service
  "Global work-stealing pool with target parallelism equal to the number of 
   cores detected. Manages concurrency of the entire project"
  (Executors/newWorkStealingPool))

(defn submit
  "Submit a job (function with no arguments) to the global 
   work-stealing pool. Returns a deref-able future.\\
   -> Future"
  [^Callable f]
  (.submit service f))

(defmacro msubmit
  "Macro version of submit that executes the body as a no argument
   function\\
   -> Future"
  [f]
  `(submit (fn [] ~f)))


(defn parse-time-unit
  "Parse a string as a time unit. Possible values:\\
   DAYS, HOURS, MINUTES, SECONDS, MICROSECONDS, MILLISECONDS, NANOSECONDS\\
   -> java.util.concurrent.TimeUnit"
  [unit]
  (condp = unit
    "DAYS" java.util.concurrent.TimeUnit/DAYS
    "HOURS" java.util.concurrent.TimeUnit/HOURS
    "MICROSECONDS" java.util.concurrent.TimeUnit/MICROSECONDS
    "MILLISECONDS" java.util.concurrent.TimeUnit/MILLISECONDS
    "MINUTES" java.util.concurrent.TimeUnit/MINUTES
    "NANOSECONDS" java.util.concurrent.TimeUnit/NANOSECONDS
    "SECONDS" java.util.concurrent.TimeUnit/SECONDS))


(defn submit-all
  "Submits all of the given no-argument functions to the global
   ExecutorService.\\
   -> List<Future>"
  ([^java.util.Collection functions timeout unit]
   (.invokeAll service 
               functions
               timeout
               (if (string? unit)
                 (parse-time-unit unit)
                 unit)))
  ([^java.util.Collection functions]
   (.invokeAll service functions)))

(defn shutdown
  "Shuts down the service"
  []
  (.shutdown service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Parallelized mapper functions   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn service-mapper
  "A parallelized mapper that uses the global ExecutorService 
   to compute the result"
  [f & colls]
  #?(:clj (let [tasks (apply mapv (fn [& args] #(apply f args)) colls)
                tasks (submit-all tasks)]
            (mapv deref tasks))
     :cljs (apply mapv f colls)))

(defn service-mapper-single
  "Slightly faster version of service-mapper for arity-1 functions"
  [f coll]
  (let [tasks (mapv (fn [c] (submit #(f c))) coll)]
    (mapv deref tasks)))

(defn future-mapper
  "A parallelized mapper that uses futures to compute the result"
  [f & colls]
  #?(:clj (let [f (fn [& args]
                    (future (apply f args)))
                tasks (apply mapv f colls)]
            (mapv deref tasks))
     :cljs (apply mapv f colls)))

(defn agent-mapper
  [f coll]
  #?(:clj (vec (let [agents (map agent coll)]
                 (dorun (map #(send % f) agents))
                 (apply await agents)
                 (mapv deref agents)))
     :cljs (mapv f coll)))

(defn agent-service-mapper
  [f coll]
  #?(:clj (vec (let [agents (map agent coll)]
                 (dorun (map #(send-via service % f) agents))
                 (apply await agents)
                 (mapv deref agents)))
     :cljs (mapv f coll)))

(defn pmap-mapper
  "Cljc-compatible version of pmap"
  [f & colls]
  #?(:clj (apply pmap f colls)
     :cljs (apply mapv f colls)))

(defn burn
  "Utility function for testing mappers \\
   Given a task-budget, returns a burner function that executes
   a number of instructions proportional to the task-budget"
  ([budget] (fn [_]
              (dotimes [i (if (< (rand) 0.1) (* 10 budget) (int (* budget 0.1)))]
                (* (int i)
                   (+ (int i)
                      (- (int i)
                         (/ (int i)
                            (inc (int i))))))))))
(defn test-mapper
  "Simulates evaluating a population of parallel-budget individuals 
   for serial-budget generations, where the error function costs task-budget
   to evaluate"
  [fn parallel-budget serial-budget task-budget]
  (let [arr1 (into [] (range parallel-budget))]
    (time (dotimes [_ serial-budget]
            (into [] (fn (burn task-budget) arr1))))))

(defmacro thrice [expression]
  `(do ~expression ~expression ~expression))

(defn test-case
  "Tests futures, agents, pmap, and service-based mappers on a given scenario"
  [parallel-budget serial-budget task-budget]
  (println "pmap")
  (thrice (test-mapper pmap-mapper parallel-budget serial-budget task-budget))
  (println "futures")
  (thrice (test-mapper future-mapper parallel-budget serial-budget task-budget))
  (println "agents")
  (thrice (test-mapper agent-mapper parallel-budget serial-budget task-budget))
  (println "service agents")
  (thrice (test-mapper agent-service-mapper parallel-budget serial-budget task-budget))
  (println "thread pool")
  (thrice (test-mapper service-mapper parallel-budget serial-budget task-budget)))

(defn test-fn 
  [input]
  ((burn 1000) 0))

#_(defn -main 
  []
  (println "pmap")
  (time (dotimes [_ 10] (into [] (pmap-mapper test-fn (range 10000)))))
  (println "futures")
  (time (dotimes [_ 10] (into [] (future-mapper test-fn (range 10000)))))
  (println "agents")
  (time (dotimes [_ 10] (into [] (agent-mapper test-fn (range 10000)))))
  (println "service")
  (time (dotimes [_ 10] (into [] (service-mapper test-fn (range 10000)))))
  #?(:clj shutdown-agents))

(def mappers 
  {"service-mapper" service-mapper
   "agent-mapper" agent-mapper 
   "pmap-mapper" pmap-mapper
   "future-mapper" future-mapper})

#_(defn -main 
  [mapper]
  (println mapper)
  (test-mapper (mappers mapper) 10000 10 1000)
  (shutdown)
  (shutdown-agents))

(defn -main
  []
  (println "Tests run on " (.availableProcessors (Runtime/getRuntime)) " processors")
  #_(println "Serial task length")
  #_(test-mapper mapv 1000 1000 10)
  #_(println "Limiting case: Negligible task budget - 1000 parallel x 1000 serial x 10 task")
  #_(test-case 1000 1000 10)
  (println "GP-like - 1000 parallel x 300 serial x 1000 task")
  (test-case 1000 10 1000)
  #_(println "Limiting case: Negligible parallel budget - 10 parallel x 1000 serial x 1000 task")
  #_(test-case 10 1000 1000)
  #_(println "Limiting case: Excessive task budget - 10 parallel x 10 serial x 100000 task")
  #_(test-case 10 10 100000)
  #_(println "Limiting case: Excessive serial budget - 10 parallel x 100000 serial x 10 task")
  #_(test-case 10 100000 10)
  #_(println "Limiting case: Excessive parallel budget - 100000 parallel x 10 serial x 10 task")
  #_(test-case 100000 10 10)
  #_(println "Similar budgets all-around - 215 parallel x 215 serial x 215 task")
  #_(test-case 215 215 215)
  #?(:clj (shutdown-agents)
     :cljs nil))

;3:35.86
