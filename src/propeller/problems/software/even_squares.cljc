;; even_squares.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given an integer 0 < n < 10000, print all of the positive even perfect
;; squares < n on separate lines.
;;
;; input stack has input integer n

(ns propeller.problems.software.even-squares
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.metrics :as metrics]
            [clojure.string :as string]
            #?(:cljs [cljs.reader :refer [read-string]]))
  (:import java.lang.Integer
           java.lang.Exception))

; Atom generators
(def instructions
  (concat (list
            ;;; end tag ERCs
            :in1
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :exec :print})))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def even-squares-data-domains
  [[(list 1 2 3 4 5 6 15 16 17 18 36 37 64 65) 14 0] ;; Small edge cases
   [(list 9600 9700 9999) 3 0] ;; Large edge cases
   [(fn [] (+ 20 (rand-int 9980))) 83 1000] ;; Random cases
   ])

;;Can make Even Squares test data like this:
;(test-and-train-data-from-domains even-squares-data-domains)

; Helper function for error function
(defn even-squares-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (let [nums (rest (take-while #(< % in)
                                                 (map #(* 4 % %)
                                                      (range))))]
               (vector in
                       (vector (apply str (interpose \newline nums))
                               nums))))
       inputs))

(defn error-helper
  [[correct-output correct-integers] output]
  (let [correct-number-lines (count correct-integers)
        result-lines (if (= output "")
                       []
                       (string/split-lines output))
        int-parse-strings (filter #(re-matches #"-?\d+" %) result-lines)
        lines-with-integer-parseable-strings (count int-parse-strings)
        lines-without-integer-parseable-strings (- (count result-lines) lines-with-integer-parseable-strings)]
    [(metrics/levenshtein-distance correct-output output)
                                     ; Error 2: Difference in number of lines with integer-parseable strings. Also, each line without an integer-parseable string contributes 1 error
     (+ (math/abs (- correct-number-lines lines-with-integer-parseable-strings))
        lines-without-integer-parseable-strings)
                                     ; Error 3: For each line in the result with a parseable integer, find the integer error compared to correct integer. Sum these.
     (let [correct-result-int-pairs (map vector
                                         correct-integers
                                         (concat (map (fn [int-str]
                                                        (try (Integer/parseInt int-str)
                                                             (catch Exception e :no-result)))
                                                      int-parse-strings)
                                                 (repeat :no-result)))]
       (apply +' (map (fn [[cor-int res-int]]
                        (if (not (number? res-int))
                          100 ; penalty for not enough lines with parseable integers
                          (math/abs (- cor-int res-int))))
                      correct-result-int-pairs)))]))

(def error-function 
  (gp/default-error-function :initialized-stacks {:print '("")}
                             :error error-helper))

(defn get-even-squares-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by first %)
       (map even-squares-test-cases
            (utils/test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def data
  (get-even-squares-train-and-test even-squares-data-domains))


(defn even-squares-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn even-squares-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Even Squares problem report - generation %s\n" generation)(flush)
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


(defn -main
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
     :step-limit              2000
     :restart 0.0
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.01
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
