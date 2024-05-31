;; pig_latin.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a string of length <= 50 containing lowercase words separated by single
;; spaces, print the string with each word translated to pig Latin. More
;; specifically, if a word starts with a vowel, it should have "ay" added to its
;; end; otherwise, the first letter is moved to the end of the word, followed by "ay".
;;
;; input stack has the input string

(ns propeller.problems.software.pig-latin
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
            [clojure.math.numeric-tower :as numeric]
            #?(:cljs [cljs.reader :refer [read-string]])
            [propeller.push.instructions :as instructions]))

;; Define test cases
(defn pig-latin-input
  "Makes a Pig Latin input of length len."
  [len]
  (apply str (interpose \space
                        (remove empty? (string/split (apply str
                                                            (repeatedly len
                                                                        (fn []
                                                                          (if (< (rand) 0.2)
                                                                            \space
                                                                            (rand-nth (map char (range 97 123)))))))
                                                     #" ")))))

; Atom generators
(def instructions
  (concat (list
            "ay"
            \space
            \a \e \i \o \u
            "aeiou"
            ;;; end constants
            (fn [] (rand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            (fn [] (pig-latin-input (rand-int 21))) ;String ERC
            ;;; end ERCs
            :in1
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:string :char :integer :boolean :exec :print})))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def pig-latin-data-domains
  [[(list "", "a", "b", "c", "d", "e", "i", "m", "o", "u", "y", "z"
          "hello", "there", "world", "eat", "apple", "yellow", "orange", "umbrella", "ouch", "in",
          "hello there world"
          "out at the plate"
          "nap time on planets"
          "supercalifragilistic"
          "expialidocious"
          (apply str (repeat 50 \u))
          (apply str (repeat 50 \s))
          (apply str (take 49 (cycle (list \w \space))))
          (apply str (take 49 (cycle (list \e \space))))
          (apply str (take 50 (cycle (list \h \a \space))))
          (apply str (take 49 (cycle (list \x \space \y \space))))) 33 0] ;; "Special" inputs covering some base cases
   [(fn [] (pig-latin-input (+ 3 (rand-int 48)))) 167 1000]
   ])

;;Can make Pig Latin test data like this:
;(test-and-train-data-from-domains pig-latin-data-domains)

; Helper function for error function
(defn pig-latin-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (apply str (interpose \space
                                       (map #(if (some #{(first %)} "aeiou")
                                               (str % "ay")
                                               (str (apply str (rest %)) (first %) "ay"))
                                           (remove empty? (string/split in #" ")))))))
       inputs))

(def error-function 
  (gp/default-error-function :input-fn (fn [input] {:in1 input})
                             :initialized-stacks {:print '("")}))

(defn get-pig-latin-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map pig-latin-test-cases
            (utils/test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def data
  (get-pig-latin-train-and-test pig-latin-data-domains))

(defn pig-latin-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn pig-latin-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Pig Latin problem report - generation %s\n" generation)(flush)
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
     :max-initial-plushy-size 500
     :step-limit              2000
     :restart 0.0
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.01
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
