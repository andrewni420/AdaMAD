;; checksum.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given a string (max length 50), compute the integer values of the characters
;; in the string, sum them, take the sum modulo 64, add the value of the \space 
;; character, and then convert that integer back into its corresponding character
;; (the checksum). Program must print "Check sum is X", where X is replaced by
;; the correct checksum.
;;
;; input stack has the input string

(ns propeller.problems.software.checksum
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.metrics :as metrics]
            [clojure.string :as string]
            [propeller.tools.maxbandit :as maxbandit]
            #?(:cljs [cljs.reader :refer [read-string]])))

; Atom generators
(def instructions
  (concat (list
            "Check sum is "
            \space
            64
           'close
            ;;; end constants
            (fn [] (- (rand-int 257) 128)) ;Integer ERC [-128,128]
            (fn [] (rand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            ;;; end tag ERCs
            :in1
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :string :char :exec :print})))


;; Define test cases
(defn checksum-input
  "Makes a checksum input of length len."
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
(def checksum-data-domains
  [[(list "", "\t", "\n", "B\n", "\n\n",
          (apply str (repeat 50 \newline))
          (apply str (repeat 50 \space))
          (apply str (repeat 50 \s))
          (apply str (take 50 (cycle (list \C \D \newline))))
          (apply str (take 50 (cycle (list \x \newline \y \space))))
          (apply str (take 50 (cycle (list \space \newline))))) 11 0] ;; "Special" inputs covering some base cases
   [(map str (map char (range 32 127))) 95 0] ; All visible characters once
   [(fn [] (checksum-input 2)) 55 500] ; Random length-2 inputs
   [(fn [] (checksum-input 3)) 50 500] ; Random length-3 inputs
   [(fn [] (checksum-input (+ 2 (rand-int 49)))) 89 1000] ; Random >= 2 length inputs
   ])

;;Can make checksum test data like this:
;(test-and-train-data-from-domains checksum-data-domains)

; Helper function for error function
(defn checksum-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (format "Check sum is %c"
                        (char (+ (mod (apply + (map int %)) 64)
                                 (int \space)))))
       inputs))

(def error-function
  (gp/default-error-function :initialized-stacks {:print '("")}
                             :input-fn (fn [input] {:in1 input})
                             :error (fn [target output]
                                      [(metrics/levenshtein-distance target output)
                                       (if (seq output)
                                         (math/abs (- (int (last target)) (int (last output)))) ;distance from correct last character
                                         1000) ;penalty for wrong format
                                       ])))


(defn get-checksum-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map checksum-test-cases
            (utils/test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def data
  (get-checksum-train-and-test checksum-data-domains))

(defn checksum-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn checksum-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Checksum problem report - generation %s\n" generation)(flush)
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
     :max-initial-plushy-size 400
     :step-limit              1500
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
                         :d [30]
                         :acc [0.03]
                         :lr [{:method :momentum
                               :momentum-lr 0.03
                               :lr 0.0003
                               :dampening 1
                              ;;  :alpha 0.0001
                              ;;  :beta1 0.1
                              ;;  :beta2 0.001
                              ;;  :warm-start 5
                               }]
                         :s [50]
                         :delta [0.01]
                         :n 100
                         :mode [:softmax-scaled]
                         :AO true
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                        ;;  :dampening 1
                        ;;  :momentum-lr [0.03]
                         :max-n 300
                        ;;  :legacy true
                         }
     :generations-per-update 1
     :reward-transformation :selection-prob-diff
     :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.01} {:e 0.03} {:e 0.1} {:e 0.3} {:e 1} {:e 3}]
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
