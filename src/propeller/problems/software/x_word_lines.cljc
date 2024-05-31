;; x_word_lines.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given an integer 1 <= X <= 10 and a string of at most 100 characters that likely
;; contains spaces and newlines, print the string with exactly X words per line.
;; The last line may have fewer than X words.
;;
;; This version uses 3 error metrics on each training case
;;
;; input stack has the input string and integer

(ns propeller.problems.software.x-word-lines
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.metrics :as metrics]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [propeller.tools.maxbandit :as maxbandit]
            [clojure.math.combinatorics :as combo]
            #?(:cljs [cljs.reader :refer [read-string]])))

; Atom generators
(def instructions
  (concat (list
            \space
            \newline
            ;;; end constants
            ;;; end tag ERCs
            :in1
            :in2
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :string :char :exec :print})))


;; Define test cases
(defn x-word-lines-input
  "Makes a X-Word Lines input of length len."
  [len]
  (apply str
         (repeatedly len
                     (fn []
                       (let [r (rand)]
                         (cond
                           (< r 0.15) \space
                           (< r 0.2) \newline
                           :else (rand-nth (map char (range 32 127)))))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def x-word-lines-data-domains
  [[(list ["" 1]
          ["" 4]
          ["A" 1]
          ["*" 6]
          [" " 7]
          ["\n" 1]
          ["s" 2]
          ["B " 1]
          ["  " 1]
          [" D" 2]
          ["2\n" 1]
          ["ef" 1]
          ["!!" 1]
          [" F " 1]
          ["T L" 1]
          ["4 s" 2]
          ["o\n&" 1]
          ["e\ne" 2]
          ["q  " 1]
          ["\n e" 1]
          ["hi " 4]
          ["q e\n" 1]
          ["  $  " 3]
          ["\n\n\nr\n" 1]
          ["9r 2 33 4" 1]
          ["9 22 3d 4r" 2]
          ["9 2a 3 4 g" 2]
          ["9 2a 3 4 g" 10]
          ["  hi   there  \n  world  lots    of\nspace         here !   \n \n" 3]
          ["Well well, what is this?\n211 days in a row that you've stopped by to see ME?\nThen, go away!" 3]
          [(apply str (take 76 (cycle (list \i \space \!)))) 4]
          [(apply str (repeat 100 \space)) 6]
          [(apply str (repeat 100 \newline)) 1]
          [(apply str (repeat 100 \s)) 7]
          [(apply str (take 100 (cycle (list \$ \space)))) 1]
          [(apply str (take 100 (cycle (list \1 \space)))) 4]
          [(apply str (take 100 (cycle (list \newline \r)))) 1]
          [(apply str (take 100 (cycle (list \newline \v)))) 10]
          [(apply str (take 100 (cycle (list \d \newline \space)))) 10]
          [(apply str (take 100 (cycle (list \H \a \space)))) 9]
          [(apply str (take 100 (cycle (list \x \space \y \!)))) 5]
          [(apply str (take 100 (cycle (list \K \space \h \newline)))) 1]
          [(apply str (take 100 (cycle (list \G \space \w \newline)))) 8]
          [(apply str (take 100 (cycle (list \space \space \3 \space \space \newline \newline \space \space)))) 3]
          [(apply str (take 100 (cycle (list \> \_ \= \])))) 2]
          [(apply str (take 100 (cycle (list \^ \_ \^ \space)))) 1]) 46 0] ; Edge case inputs
   [(fn [] [(x-word-lines-input (inc (rand-int 100)))
            (rand-nth (concat (range 1 11) (range 1 6) (range 1 4)))]) 104 2000] ; Random inputs. For X, [1,3] will have 1/6 chance each, [4,5] will have 1/9 chance each, and [6,10] will have 1/18 chance each
   ])

;;Can make X-Word Lines test data like this:
;(test-and-train-data-from-domains x-word-lines-data-domains)

; Helper function for error function
(defn x-word-lines-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output] where input is [in-str in-int] and output is [out-str out-int]"
  [inputs]
  (map (fn [[in-str in-int]]
         (vector [in-str in-int]
                 [(apply str
                        (flatten (interpose (list \newline)
                                           (map #(interpose \space %)
                                                (partition-all in-int (string/split (string/trim in-str) #"\s+"))))))
                  in-int]))
       inputs))

(def error-function
  (gp/default-error-function :initialized-stacks {:print '("")}
                             :error (fn [[target input2] output]
                                      [; First error is Levenshtein distance of printed strings
                                       (metrics/levenshtein-distance target output)
                                                                    ; Second error is integer distance from the correct number of newlines
                                       (math/abs (- (count (filter #(= % \newline) target))
                                                    (count (filter #(= % \newline) output))))
                                                                    ; Third error is summed error of integer distances over the lines of the correct number of words per line
                                       (+ (apply + (map #(math/abs (- input2
                                                                      (count (string/split (string/trim %) #"\s+"))))
                                                        (butlast (string/split-lines output))))
                                          (math/abs (- (count (string/split (string/trim (last (string/split-lines target))) #"\s+"))
                                                       (count (string/split (string/trim (let [last-line (last (string/split-lines output))]
                                                                                           (if last-line last-line "")))
                                                                            #"\s+")))))])))
                         
(defn get-x-word-lines-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first first) %)
       (map x-word-lines-test-cases
            (utils/test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def data
  (get-x-word-lines-train-and-test x-word-lines-data-domains))

(defn x-word-lines-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn x-word-lines-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- X-Word Lines problem report - generation %s\n" generation)(flush)
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
     :step-limit              1600
     :restart 0.0
     :mapper mapv
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.1
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
     :max-initial-plushy-size 400
     :step-limit              1600
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
                         :lr [{:method :adam
                              ;;  :momentum-lr 0.03
                              ;;  :lr 0.0003
                              ;;  :dampening 1
                               :alpha 0.0003
                               :beta1 0.03
                               :beta2 0.001
                              ;;  :warm-start 5
                               }]
                         :s [50]
                         :delta [0.01]
                         :n 300
                         :mode [:softmax-scaled]
                         :AO false
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                        ;;  :dampening 1
                        ;;  :momentum-lr [0.03]
                         :max-n 300
                         :sigma 2
                        ;;  :legacy true
                         }
     :generations-per-update 1
     :reward-transformation :log-diff
     :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.01} {:e 0.03} {:e 0.1} {:e 0.3} {:e 1} {:e 3}]
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
