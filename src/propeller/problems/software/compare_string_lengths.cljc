;; compare_string_lengths.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given three strings in1, in2, and in3, return true if
;; lenth(in1) < length(in2) < length(in3), and false otherwise.
;;
;; input stack has 3 input strings

(ns propeller.problems.software.compare-string-lengths
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.metrics :as metrics]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])))

; Atom generators
(def instructions
  (concat (list
            (fn [] (rand-nth (list true false))) ;Boolean ERC
            ;;; end tag ERCs
            :in1
            :in2
            :in3
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :string :exec})))


;; Define test cases
(defn csl-input
  "Makes a Compare String Lengths input string of length len."
  [len]
  (apply str
         (repeatedly len
                     #(rand-nth (concat [\newline \tab]
                                         (map char (range 32 127)))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def csl-data-domains
  [[(list ["" "" ""]) 1 0] ;; All empty strings
   [(combo/permutations ["" "a" "bc"]) 6 0] ;; Permutations of three small strings
   [(apply concat (repeatedly 2 #(combo/permutations ["" "" (csl-input (inc (rand-int 49)))]))) 6 0] ;; Cases with 2 empties and a non-empty
   [(apply concat (repeatedly 3 #(combo/permutations (conj (repeat 2 (csl-input (inc (rand-int 49)))) (csl-input (inc (rand-int 49))))))) 9 0] ;; Cases with 2 strings repeated
   [(fn [] (repeat 3 (csl-input (rand-int 50)))) 3 100] ;; Cases where all are the same
   [(fn [] (sort-by count (repeatedly 3 #(csl-input (rand-int 50))))) 25 200] ;; Cases forced to be in order (as long as two aren't same size randomly, will be true)
   [(fn [] (repeatedly 3 #(csl-input (rand-int 50)))) 50 700] ;; Cases in random order
   ])

;;Can make Compare String Lengths test data like this:
;(test-and-train-data-from-domains csl-data-domains)

; Helper function for error function
(defn csl-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (apply < (map count %)))
       inputs))



#_(defn error-function
  [argmap data individual]
  (let [program (genome/plushy->push (:plushy individual) argmap)
        inputs (map first data)
        targets (map second data)
        outputs (map (fn [[in1 in2 in3]]
                       (let [stack (interpreter/interpret-program
                                    program
                                    (assoc state/empty-state :input {:in1 in1 
                                                                     :in2 in2 
                                                                     :in3 in3})
                                    (:step-limit argmap))]
                         (state/peek-stack stack :boolean)))
                     inputs)
        errors (map (fn [target output]
                         (if (= target output) 0 1))
                       targets
                       outputs)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error #?(:clj  (apply +' errors)
                           :cljs (apply + errors)))))

(def error-function 
  (gp/default-error-function :out-stacks :boolean
                             :error (fn [target output]
                                      (if (= target output) 0 1))))



(defn get-compare-string-lengths-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map csl-test-cases
       (utils/test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def data
  (get-compare-string-lengths-train-and-test csl-data-domains))

(defn compare-string-lengths-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn csl-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Compare String Lengths problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


#_(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (gp/gp
   (merge
    {:instructions            instructions
     :error-function          error-function
     :training-data           (first data)
     :testing-data            (second data)
     :max-generations         300
     :population-size         1000
     :max-initial-plushy-size 200
     :step-limit              600
     :restart 0.0
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.01
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))


(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (maxbandit/gp
   (merge
    {#_:mapper #_mapv
     :instructions            instructions
     :error-function          error-function
     :training-data           (first data)
     :testing-data            (second data)
     :max-generations         300
     :population-size         [1000 1000]
     :max-initial-plushy-size 100
     :step-limit              200
     :parent-selection        :lexicase
     :tournament-size         5
     :update-freq 100
     :report-interval 1
     :transformations [maxbandit/tile-to-parameters-umad maxbandit/parameters-to-tile-umad]
     :bandit-parameters {:num-bandits 1
                         :l [-10]
                         :r [0]
                         :to [[0 0.03 0.06 0.09 0.12 0.15]] #_[[0 0.03 0.06 0.09 0.12 0.15 0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39 0.42 0.45 0.48]]
                         :ta [[0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39]] #_[[0.51 0.54 0.57 0.6 0.63 0.66 0.69 0.72 0.75 0.78 0.81 0.84 0.87 0.9]]
                         :num-codings 20
                         :d [1]
                         :acc [0.03]
                         :lr [{:method :sgd
                              ;;  :momentum-lr 0.03
                               :lr 0.01
                              ;;  :dampening 1
                              ;;  :alpha 0.0001
                              ;;  :beta1 0.1
                              ;;  :beta2 0.001
                              ;;  :warm-start 5
                               }]
                         :s [50]
                         :delta [0.01]
                         :n 100
                         :mode [:argmax]
                         :AO true
                         :xi* [0.01]
                         :epsilon [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                        ;;  :dampening 1
                        ;;  :momentum-lr [0.03]
                         :max-n 300
                        ;;  :legacy true
                         :sigma 2}
     :generations-per-update 1
     :reward-transformation :log-diff
     :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.01} {:e 0.03} {:e 0.1} {:e 0.3} {:e 1} {:e 3}]
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
