;; grade.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given 5 integer inputs, all in range [0,100]. The first four represent the
;; lower numeric thresholds for achieving an A, B, C, and D, and will be
;; distinct and in descending order. The fifth represents the student's numeric
;; grade. The program must output "Student has a X grade.", where X is A, B, C,
;; D, or F depending on the thresholds and their numeric grade.
;;
;; input stack has 5 integers

(ns propeller.problems.software.grade
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
            #?(:cljs [cljs.reader :refer [read-string]])
            [propeller.push.instructions :as instructions]
            [propeller.tools.concurrent :as concurrent]))

; Atom generators
(def instructions
  (concat (list
            "Student has a "
            " grade."
            "A"
            "B"
            "C"
            "D"
            "F"
            ;;; end constants
            (fn [] (rand-int 101)) ;Integer ERC [0,100]
            ;;; end tagERCs
            :in1
            :in2
            :in3
            :in4
            :in5
           'close
            ;;; end input instructions
            )
          (get-stack-instructions #{:integer :boolean :string :exec :print})))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def grade-data-domains
  [[(list '(80 70 60 50 85)
          '(80 70 60 50 80)
          '(80 70 60 50 79)
          '(80 70 60 50 75)
          '(80 70 60 50 70)
          '(80 70 60 50 69)
          '(80 70 60 50 65)
          '(80 70 60 50 60)
          '(80 70 60 50 59)
          '(80 70 60 50 55)
          '(80 70 60 50 50)
          '(80 70 60 50 49)
          '(80 70 60 50 45)) 13 0] ;; Cases from Repair Benchmark paper
   [(list '(90 80 70 60 100)
          '(90 80 70 60 0)
          '(4 3 2 1 5)
          '(4 3 2 1 4)
          '(4 3 2 1 3)
          '(4 3 2 1 2)
          '(4 3 2 1 1)
          '(4 3 2 1 0)
          '(100 99 98 97 100)
          '(100 99 98 97 99)
          '(100 99 98 97 98)
          '(100 99 98 97 97)
          '(100 99 98 97 96)
          '(98 48 27 3 55)
          '(98 48 27 3 14)
          '(98 48 27 3 1)
          '(45 30 27 0 1)
          '(45 30 27 0 0)
          '(48 46 44 42 40)
          '(48 46 44 42 41)
          '(48 46 44 42 42)
          '(48 46 44 42 43)
          '(48 46 44 42 44)
          '(48 46 44 42 45)
          '(48 46 44 42 46)
          '(48 46 44 42 47)
          '(48 46 44 42 48)
          '(48 46 44 42 49)) 28 0] ;; Hand-written cases
   [(fn []
      (let [thresholds (sort > (repeatedly 4 #(rand-int 101)))]
        (if (apply distinct? thresholds)
          (concat thresholds (list (rand-int 101)))
          (recur)))) 159 2000] ;; Random cases, which make sure that first 4 integers are distinct
   ])

;;Can make Grade test data like this:
;(test-and-train-data-from-domains grade-data-domains)

; Helper function for error function
(defn grade-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2 input3 input4 input5] output]."
  [inputs]
  (map #(vector %
                (str "Student has a "
                     (let [score (last %)]
                       (cond
                         (>= score (first %)) \A
                         (>= score (second %)) \B
                         (>= score (nth % 2)) \C
                         (>= score (nth % 3)) \D
                         :else \F))
                     " grade."))
       inputs))

(def error-function 
  (gp/default-error-function :initialized-stacks {:print '("")}
                             :error (fn [target output] 
                                      [(metrics/levenshtein-distance target output)
                                      (let [printed-letter (second (re-find #"^Student has a (.) grade.$" output))
                                            correct-letter (second (re-find #"^Student has a (.) grade.$" target))]
                                        (if printed-letter
                                          (math/abs (- (int (first correct-letter))
                                                  (int (first printed-letter)))) ;distance from correct character
                                          1000))])))

(defn get-grade-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map grade-test-cases
       (utils/test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def data
  (get-grade-train-and-test grade-data-domains))


(defn grade-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first data))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second data))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn grade-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Grade problem report - generation %s\n" generation)(flush)
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
     :step-limit              800
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
    {:mapper concurrent/service-mapper
    ;;  :downsample?                 true ; wether to use downsampling
    ;;  :ds-function                 :case-maxmin ;; :case-maxmin ; :case-rand, case-maxmin, case-maxmin-auto
    ;;  :downsample-rate             0.05 ; proportion of data used in downsample
    ;;  :ds-parent-rate              0.01 ; proportion of parents used to evaluate case distances
    ;;  :ds-parent-gens              10 ; generations between computation of parent distances
     :instructions            instructions
     :error-function          error-function
     :training-data           (first data)
     :testing-data            (second data)
     :max-generations         300
     :population-size         [1000 1000]
     :max-initial-plushy-size 200
     :step-limit              800
     :parent-selection        :lexicase
     :tournament-size         5
     :update-freq 1000
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
                         :lr [{:method :momentum
                               :momentum-lr 0.1
                               :lr 0.001
                               :dampening 1
                                                      ;;  :alpha 0.0003
                                                      ;;  :beta1 0.03
                                                      ;;  :beta2 0.001
                                                      ;;  :warm-start 5
                               }]
                         :s [50]
                         :delta [0.01]
                         :n 300
                         :mode [:argmax]
                         :AO false
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 5995 0)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 5995 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                                                ;;  :dampening 1
                                                ;;  :momentum-lr [0.03]
                         :max-n 100
                         :sigma 3
                                                ;;  :legacy true
                         }
     :generations-per-update 1
     :reward-transformation :log-diff
    ;;  :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.0001} {:e 0.0003} {:e 0.001} {:e 0.003} {:e 0.01} {:e 0.03} {:e 0.1}]
     :custom-report maxbandit/custom-report
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))
