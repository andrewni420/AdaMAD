;; string_lengths_backwards.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of strings with length <= 50, where each string has
;; length <= 50, print the length of each string in the vector starting with
;; the last and ending with the first.
;;
;; input stack has 1 input vector of strings

(ns propeller.problems.software.string-lengths-backwards
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
            #?(:cljs [cljs.reader :refer [read-string]])
            [propeller.push.instructions :as instructions]))

; Atom generators
(def instructions
  (concat (list
            (fn [] (- (rand-int 201) 100)) ; Integer ERC in [-100,100]
            ;;; end tag ERCs
            :in1
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:string :vector_string :integer :boolean :exec :print})))

;; Define test cases
(defn string-generator
  "Makes a random string of length len."
  [len]
  (apply str
         (repeatedly len
                     #(rand-nth (concat [\newline \tab]
                                         (map char (range 32 127)))))))

(defn string-lengths-input
  "Makes a String Lengths input vector of length len."
  [len]
  (vec (repeatedly len
                   #(string-generator (rand-int 51)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def string-lengths-data-domains
  [[(list []) 1 0] ;; Empty input vector
   [(list [""]
          ["" ""]
          ["" "" ""]
          ["" "" "" "" "" "" "" "" "" ""]) 4 0] ;; Vectors with empty strings
   [(list ["abcde"]
          ["1"]
          ["abc" "hi there"]
          ["!@#" "\n\n\t\t" "5552\na r"]
          ["tt" "333" "1" "ccc"]) 5 0] ;; Vectors with small numbers of inputs
   [(fn [] (string-lengths-input (inc (rand-int 50)))) 90 1000] ;; Random vectors
   ])

;;Can make String Lengths test data like this:
#_(test-and-train-data-from-domains string-lengths-data-domains)

; Helper function for error function
(defn string-lengths-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (apply str (interpose \newline (reverse (map count %)))))
       inputs))

(def error-function 
  (gp/default-error-function :input-fn (fn [input] {:in1 input})
                             :initialized-stacks {:print '("")}))

(defn get-string-lengths-backwards-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map string-lengths-test-cases
       (utils/test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def data
  (get-string-lengths-backwards-train-and-test string-lengths-data-domains))

(defn string-lengths-backwards-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn string-lengths-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- String Lengths problem report - generation %s\n" generation)(flush)
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
     :max-initial-plushy-size 150
     :step-limit              600
     :restart 0.0
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.01
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))


;lexicase: 74 successes, 65 generalizing