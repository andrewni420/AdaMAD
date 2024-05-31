;; super_anagrams.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given strings x and y of lowercase letters with length <= 20, return true if
;; y is a super anagram of x, which is the case if every character in x is in y.
;; To be true, y may contain extra characters, but must have at least as many
;; copies of each character as x does.
;;
;; input stack has the 2 input strings
;;

(ns propeller.problems.software.super-anagrams
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
            #?(:cljs [cljs.reader :refer [read-string]])))

; Atom generators
(def instructions
  (concat (list
            ;;; end constants
            (fn [] (rand-nth (list true false))) ;Boolean ERC
            (fn [] (- (rand-int 2001) 1000)) ;Integer ERC [-1000,1000]
            (fn [] (rand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            ;;; end tag ERCs
            :in1
            :in2
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:string :char :integer :boolean :exec})))


;; Define test cases
(defn super-anagrams-input
  "Makes a pair of Super Anagrams inputs."
  []
  (let [len (inc (rand-int 20))
        input1 (apply str
                      (repeatedly len
                                  (fn []
                                    (rand-nth (map char (range 97 123))))))
        change-char-sometimes-fn (fn [c]
                                   ;Each char has 10% chance of being replaced
                                   (if (< (rand) 0.1)
                                     (rand-nth (map char (range 97 123)))
                                     c))
        num-chars-to-drop (rand-int len)
        input2 (apply str (drop num-chars-to-drop
                                (shuffle (map change-char-sometimes-fn input1))))]
    (if (< (rand) 0.2) ;;Choose random order (biased towad input2 first) since len(input1) >= len(input2)
      [input1 input2]
      [input2 input1])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def super-anagrams-data-domains
  [[(list ["" ""]
          ["" "h"]
          ["i" ""]
          ["a" "a"]
          ["b" "c"]
          ["n" "nn"]
          ["abcde" "c"]
          ["c" "abcde"]
          ["r" "mnbvccxz"]
          ["abc" "aabc"]
          ["aabc" "abcde"]
          ["abcde" "edcba"]
          ["mo" "moo"]
          ["moo" "mo"]
          ["tree" "though"]
          ["rip" "zipper"]
          ["flipper" "rip"]
          ["hi" "zipper"]
          ["dealer" "dollars"]
          ["loud" "louder"]
          ["ccccccccc" "ccccc"]
          ["clinteastwood" "oldwestaction"]
          ["clinteastwood" "ldwestaction"]
          ["verificationcomplete" "verificationcomplete"]
          ["hahahahahahahahahaha" "hhhhhhhhhhaaaaaaaaaa"]
          ["hahahahahahahahahaha" "aahhhh"]
          ["" "qwqeqrqtqyquqiqoqpqs"]
          ["wxyz" "qazwsxedcrfvtgbyhnuj"]
          ["dddeeefffgggg" "gggffggfefeededdd"]
          ["gggffggfefeededdd" "dddeeefffgggg"]) 30 0] ; hand-written cases
   [super-anagrams-input 170 2000] ; Random inputs designed to be close to anagrams
   ])

;;Can make test data like this:
;(test-and-train-data-from-domains super-anagrams-data-domains)

; Helper function for error function
(defn super-anagrams-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[in1 in2]]
         (vector [in1 in2]
                 (loop [i1 in1
                        i2 in2]
                   (cond
                     (empty? i1) true
                     (> 0 (.indexOf i2 (str (first i1)))) false
                     :else (recur (rest i1)
                                  (string/replace-first i2 (first i1) \space))))))
       inputs))

(def error-function 
  (gp/default-error-function :initialized-stacks {:print '("")}
                             :out-stacks :boolean
                             :error (fn [target output]
                                      (if (= target output)
                                        0
                                        1))))

(defn get-super-anagrams-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by second %)
       (map super-anagrams-test-cases
            (utils/test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def data
  (get-super-anagrams-train-and-test super-anagrams-data-domains))

(defn super-anagrams-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn super-anagrams-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Super Anagrams problem report - generation %s\n" generation)(flush)
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
     :max-initial-plushy-size 400
     :step-limit              1600
     :restart 0.0
     :parent-selection        :lexicase
     :tournament-size         5
     :umad-rate               0.01
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
