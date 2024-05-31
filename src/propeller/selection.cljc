(ns propeller.selection
  "Propeller includes many kinds of genetic operators to select parents within the population such as tournament selection,
  lexicase selection, and epsilon lexicase selection."
  {:doc/format :markdown}
  (:require [propeller.tools.math :as math-tools]
            [clojure.pprint :as pprint]
            [clojure.set]
            [propeller.tools.distributions :as dist]
            [clojure.set :as set]
            [propeller.tools.math :as math])
  (:import java.lang.System))

(defn tournament-selection
  "Selects an individual from the population using tournaments of
  tournament-size by taking the individual in the tournament with the lowest :total-error. "
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))

(defn truncation-selection 
  "Truncation selection. If (:truncation-size argmap) is an integer, interprets it as number of individuals to truncate at. 
   Otherwise interprets it as a proportion of the population to truncate at."
  [pop argmap]
  (let [truncation-size (:truncation-size argmap)
        truncation-size (if (int? truncation-size) truncation-size (int (* (count pop) truncation-size)))
        truncated-set (take truncation-size (sort-by :total-error pop))]
    (rand-nth truncated-set)))



(defn inherited-lexicase
  "Selects an individual from the population using lexicase selection.
  Lexicase parent selection filters the population by considering one random training case at a time,
  eliminating any individuals with errors for the current case that are worse than the best error in the selection pool,
  until a single individual remains."
  [pop argmap & {:keys [cases maximize verbose]}]
  (let [make-range #(range (count %))]
    (loop [survivors (map rand-nth (vals (group-by :inherited-errors pop)))
           cases (or cases (map (comp shuffle make-range) (:inherited-errors (first pop))))
           i 0]
      (when (and verbose (= 0 i)) (println "cases " cases) (println "errors " (map :inherited-errors survivors)))
      (if (or (empty? cases)
              (empty? (rest survivors)))
        ((fn [s] (when verbose (println (:inherited-errors s))) s) (rand-nth survivors))
        (recur
         (loop [survivors survivors
                ordering (first cases)]
           (if (or (empty? ordering)
                   (empty? (rest survivors)))
             survivors
             (let [min-err-for-case (apply (if maximize max min)
                                           (map (comp #(nth % (first ordering)) #(nth % i) :inherited-errors) survivors))]
               (recur (filter #(= (nth (nth (:inherited-errors %) i) (first ordering)) min-err-for-case)
                              survivors)
                      (rest ordering)))))
         (rest cases)
         (inc i))))))

(defn lexicase-selection
  "Selects an individual from the population using lexicase selection.
  Lexicase parent selection filters the population by considering one random training case at a time,
  eliminating any individuals with errors for the current case that are worse than the best error in the selection pool,
  until a single individual remains."
  [pop argmap & {:keys [cases maximize]}]
  (if (:inherit-depth argmap)
    (inherited-lexicase pop argmap :cases cases :maximize maximize)
    (loop [survivors (map rand-nth (vals (group-by :errors pop)))
           cases (or cases (shuffle (range (count (:errors (first pop))))))]
      (if (or (empty? cases)
              (empty? (rest survivors)))
        (rand-nth survivors)
        (let [min-err-for-case (apply (if maximize max min) (map #(nth % (first cases))
                                                                 (map :errors survivors)))]
          (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                         survivors)
                 (rest cases)))))))

(defn lexicase-by-errors
  "Takes an popsize x casecount error matrix, and returns the index of the 
   individual selected by lexicase selection. Assumes pre-optimization for repeated error vectors"
  [errors & {:keys [cases maximize]}]
  (loop [indices (range (count errors))
         cases (or cases (shuffle (range (count (first errors)))))]
    (if (or (empty? cases) (empty? (rest indices)))
      (rand-nth indices)
      (let [min-err-for-case (apply (if maximize max min)
                                    (map #(nth (nth errors %) (first cases))
                                         indices))]
        (recur (filter #(= (nth (nth errors %) (first cases)) min-err-for-case)
                        indices)
                (rest cases))))))


(defn lexicase-distribution
  "Selects an individual from the population using lexicase selection.
  Lexicase parent selection filters the population by considering one random training case at a time,
  eliminating any individuals with errors for the current case that are worse than the best error in the selection pool,
  until a single individual remains.
  Returns {groups probabilities indices cases} where 
  groups = {err: [ind]}, probabilities = {idx of err in (keys groups):prob}, indices = [idx of err in (keys groups)], and cases = [[test case]]"
  [pop iterations & {:keys [maximize mapper] :or {mapper mapv}}]
  (let [groups (group-by :errors pop)
        error-classes (keys groups)
        n (->> error-classes (first) (count))
        cases (into [] (repeatedly iterations #(shuffle (range n))))
        indices (mapper #(lexicase-by-errors error-classes :cases % :maximize maximize) cases)
        probabilities (into {} (map #(vector (first %) (/ (second %) iterations))) (frequencies indices))
        logerrs (mapv (fn [c] (/ (transduce (map #(- (math/log (+ 1 %)))) + 0 c) n)) error-classes)]
    {:groups groups :probabilities probabilities :indices indices :cases cases :logerrs logerrs}))


(defn assess-lexicase-prob-legacy
  "Calculates the probability of selecting the given error vector given the
   distribution {groups probabilities indices cases} calculated by lexicase-distribution"
  [error distribution]
  (let [{groups :groups probs :probabilities indices :indices cases :cases} distribution
        group-counts (map count (vals groups))]
    (if (contains? groups error)
      (let [total (reduce + group-counts)]
        (/ (count (get groups error)) total))
      (let [error-classes (keys groups)
            success? (fn [e2 other-count cases]
                       (loop [cases cases]
                         (if (empty? cases)
                           (/ 1 (+ 1 other-count))
                           (let [c (first cases)
                                 h1 (nth error c)
                                 h2 (nth e2 c)]
                             (cond (> h1 h2) 0
                                   (< h1 h2) 1
                                   (= h1 h2) (recur (rest cases)))))))]
        (math/mean (map (partial apply success?)
                        (map (fn [idx c]
                               [(nth error-classes idx) (nth group-counts idx) c])
                             indices
                             cases)))))))

(defn assess-lexicase-prob
  "Calculates the probability of selecting the given error vector given the
   distribution {groups probabilities indices cases} calculated by lexicase-distribution.
   Optionally multiplies it by a weight"
  [error distribution & {:keys [ind-weight]
                         :or {ind-weight 1}}]
  (let [{groups :groups probs :probabilities indices :indices cases :cases} distribution
        group-counts (mapv count (vals groups))]
    (if (contains? groups error)
      (let [idx (first (keep-indexed #(when (= %2 error) %1) (keys groups)))]
        (/ (get probs idx 0) (nth group-counts idx)))
      (let [error-classes (keys groups)
            success? (fn [e2 other-count cases]
                       (loop [cases cases]
                         (if (empty? cases)
                           (/ 1 (+ 1 other-count))
                           (let [c (first cases)
                                 h1 (nth error c)
                                 h2 (nth e2 c)]
                             (cond (> h1 h2) 0
                                   (< h1 h2) 1
                                   (= h1 h2) (recur (rest cases)))))))]
        (* ind-weight
           (math/mean (map (partial apply success?)
                           (map (fn [idx c]
                                  [(nth error-classes idx) (nth group-counts idx) c])
                                indices
                                cases))))))))


(defn in-group-prob-diff
  "Calculate difference in lexicase selection probability when both error vectors are in the population
   of the lexicase selection distribution"
  [error parent distribution & {:keys [parent-weight ind-weight]
                                :or {parent-weight 1
                                     ind-weight 1}}]
  (let [{groups :groups probs :probabilities} distribution
        group-counts (mapv count (vals groups))
        err-idx (first (keep-indexed #(when (= %2 error) %1) (keys groups)))
        parent-idx (first (keep-indexed #(when (= %2 parent) %1) (keys groups)))
        err-prob (get probs err-idx 0)
        parent-prob (get probs parent-idx 0)]
    (if (= err-idx parent-idx)
      0
      (- (* ind-weight (/ err-prob (inc (get group-counts err-idx))))
         (* parent-weight (/ parent-prob (get group-counts parent-idx)))))))



(defn half-in-group-prob-diff 
  "Calculate difference in lexicase selection probability when one of the error vectors are in the population
   of the lexicase selection distribution. Assumes error2 is in the distribution and returns P(error2)-P(error1)"
  [error1 error2 distribution & {:keys [parent-weight ind-weight]
                                :or {parent-weight 1
                                     ind-weight 1}}]
  (let [{groups :groups indices :indices cases :cases} distribution
        error-classes (keys groups)
        group-counts (mapv count (vals groups))
        success? (fn [e2 other-count cases]
                   (loop [cases cases]
                     (if (empty? cases)
                       (do (println "NOT SUPPOSED TO RUN OUT OF CASES SUCCESS") (/ 1 (inc other-count)))
                       (let [c (first cases)
                             h1 (nth error1 c)
                             h2 (nth e2 c)]
                         (cond (> h1 h2) 0
                               (< h1 h2) 1
                               (= h1 h2) (recur (rest cases)))))))
        parent-success? (fn [e other-count success]
                          (if (= error2 e)
                            (condp = success
                              0 (/ 1 other-count)
                              1 0
                              (do (println "NOT SUPPOSED TO RUN OUT OF CASES PARENT-SUCCESS") (/ 1 (inc other-count))))
                            0))
        successes (mapv (partial apply success?)
                        (map (fn [idx c]
                               [(nth error-classes idx) (nth group-counts idx) c])
                             indices
                             cases))
        parent-successes (mapv (partial apply parent-success?)
                               (map (fn [idx s]
                                      [(nth error-classes idx) (nth group-counts idx) s])
                                    indices
                                    successes))]
    (- (* ind-weight (math/mean successes)) 
       (* parent-weight (math/mean parent-successes)))))
  

(defn half-in-group-prob-diff-weighted
  "Calculate difference in lexicase selection probability when one of the error vectors are in the population
   of the lexicase selection distribution. Assumes error2 is in the distribution and returns P(error2)-P(error1)"
  [error1 error2 distribution & {:keys [parent-weight ind-weight]
                                :or {parent-weight 1
                                     ind-weight 1}}]
  (let [{groups :groups indices :indices cases :cases} distribution
        error-classes (keys groups)
        group-counts (mapv count (vals groups))
        success? (fn [e2 other-count cases]
                   (loop [cases cases]
                     (if (empty? cases)
                       (do (println "NOT SUPPOSED TO RUN OUT OF CASES SUCCESS") (/ 1 (inc other-count)))
                       (let [c (first cases)
                             h1 (nth error1 c)
                             h2 (nth e2 c)]
                         (cond (> h1 h2) 0
                               (< h1 h2) 1
                               (= h1 h2) (recur (rest cases)))))))
        parent-success? (fn [e other-count success]
                          (if (= error2 e)
                            (condp = success
                              0 (/ 1 other-count)
                              1 0
                              (do (println "NOT SUPPOSED TO RUN OUT OF CASES PARENT-SUCCESS") (/ 1 (inc other-count))))
                            0))
        successes (mapv (partial apply success?)
                        (map (fn [idx c]
                               [(nth error-classes idx) (nth group-counts idx) c])
                             indices
                             cases))
        parent-successes (mapv (partial apply parent-success?)
                               (map (fn [idx s]
                                      [(nth error-classes idx) (nth group-counts idx) s])
                                    indices
                                    successes))]
    (- (* ind-weight (math/mean successes)) 
       (* parent-weight (math/mean parent-successes)))))

(defn not-in-group-prob-diff
  "Calculate difference in lexicase selection probability when neither of the error vectors are in the population
     of the lexicase selection distribution"
  [error parent distribution & {:keys [parent-weight ind-weight]
                                :or {parent-weight 1
                                     ind-weight 1}}]
  (let [{groups :groups indices :indices cases :cases} distribution
        error-classes (keys groups)
        successes (mapv #(lexicase-by-errors [parent (nth error-classes %1) error]
                                             :cases %2)
                        indices
                        cases)]
    (math/mean (mapv #(condp = % 2 ind-weight 0 (- parent-weight) 0) successes))))


(defn prob-weighted-log
  "Calculate difference in lexicase selection probability when neither of the error vectors are in the population
     of the lexicase selection distribution"
  [error distribution]
  (let [ind-logerr (/ (transduce (map #(- (math/log (inc %)))) + 0 error) (count error))
        {groups :groups indices :indices cases :cases logerrs :logerrs} distribution
        error-classes (keys groups)]
    (if (contains? groups error)
      0
      (let [successes (mapv #(lexicase-by-errors [(nth error-classes %1) error]
                                                 :cases %2)
                            indices
                            cases)
            pwl (math/mean (mapv (fn [s i]
                                   (if (= s 1)
                                     (- ind-logerr (nth logerrs i))
                                     0)) successes indices))]
        ;; (pprint/pprint {:prob-weighted-log pwl})
        pwl))))



(defn assess-lexicase-difference
  "Calculates the probability of selecting the given error vector given the
   distribution {groups probabilities indices cases} calculated by lexicase-distribution"
  [error parent distribution & {:keys [parent-weight ind-weight]
                                :or {parent-weight 1 
                                     ind-weight 1}}]
  (let [{groups :groups} distribution
        error-in-group (contains? groups error)
        parent-in-group (contains? groups parent)]
    (cond  
      (nil? parent) (assess-lexicase-prob error distribution :ind-weight ind-weight)
      (and error-in-group parent-in-group) (in-group-prob-diff error parent distribution :parent-weight parent-weight :ind-weight ind-weight)
      error-in-group (- (half-in-group-prob-diff parent error distribution :parent-weight ind-weight :ind-weight parent-weight))
      parent-in-group (half-in-group-prob-diff error parent distribution :parent-weight parent-weight :ind-weight ind-weight)
      :else (not-in-group-prob-diff error parent distribution :parent-weight parent-weight :ind-weight ind-weight))))


#_(defn assess-lexicase-difference
  "Calculates the probability of selecting the given error vector given the
   distribution {groups probabilities indices cases} calculated by lexicase-distribution"
  [error parent distribution]
  (let [{groups :groups probs :probabilities indices :indices cases :cases} distribution
        group-counts (mapv count (vals groups))]
    (if (contains? groups error)
      (let [err-idx (first (keep-indexed #(when (= %2 error) %1) (keys groups)))
            parent-idx (first (keep-indexed #(when (= %2 parent) %1) (keys groups)))
            err-prob (get probs err-idx 0)
            parent-prob (get probs parent-idx 0)]
        (if (= err-idx parent-idx)
          0
          (- (/ err-prob (inc (get group-counts err-idx))) (/ parent-prob (get group-counts parent-idx)))))
      (let [error-classes (keys groups)
            success? (fn [e2 other-count cases]
                       (loop [cases cases]
                         (if (empty? cases)
                           (do (println "NOT SUPPOSED TO RUN OUT OF CASES SUCCESS") (/ 1 (inc other-count)))
                           (let [c (first cases)
                                 h1 (nth error c)
                                 h2 (nth e2 c)]
                             (cond (> h1 h2) 0
                                   (< h1 h2) 1
                                   (= h1 h2) (recur (rest cases)))))))
            parent-success? (fn [e other-count success]
                              (if (= parent e)
                                (condp = success
                                  0 (/ 1 other-count)
                                  1 0
                                  (do (println "NOT SUPPOSED TO RUN OUT OF CASES PARENT-SUCCESS") (/ 1 (inc other-count))))
                                0))
            successes (mapv (partial apply success?)
                            (map (fn [idx c]
                                   [(nth error-classes idx) (nth group-counts idx) c])
                                 indices
                                 cases))
            parent-successes (mapv (partial apply parent-success?)
                                   (map (fn [idx s]
                                          [(nth error-classes idx) (nth group-counts idx) s])
                                        indices
                                        successes))]
        (- (math/mean successes) (math/mean parent-successes))))))

(defn test-test
    []
  (let [dist (lexicase-distribution [{:errors [1 2 3]}
                                     {:errors [1 0 1]}
                                     {:errors [0 1 1]}
                                     {:errors [3 1 0]}
                                     {:errors [4 0 0]}
                                     {:errors [2 2 2]}]
                                    10)]
    (pprint/pprint dist)
    (let [success (assess-lexicase-difference [1 0 0] [0 1 1] dist)]
      (pprint/pprint success))))

(defn meta-selection-gesmr
  "Meta selection of mutation rates from the group elites paper.
   Returns [elite-idx, [non-elite-idx]]"
  [best-changes eta]
  (let [num-selected (if (int? eta) 
                       eta
                       (int (* eta (count best-changes))))
        sorted (->> best-changes 
                    (count)
                    (range)
                    (shuffle)
                    (sort-by (partial nth best-changes) >)
                    (take num-selected))
        elite (first sorted)
        non-elite (into [] (rest sorted))
        new-rate #(rand-nth non-elite)]
    [elite, (into [] (repeatedly (dec (count best-changes)) new-rate))]))

(defn ensure-nonzero
  [f]
  (loop [r (f)]
    (if (zero? r)
      (recur (f))
      r)))

(def exponent-sampler
  {:uniform #(ensure-nonzero (fn [] (+ (rand (- %2 %1)) %1)))
   :normal #(dist/rand-normal %1 %2)
   :log-uniform (fn [low high & rest] ((if (empty? rest) identity (partial * (rand-nth [-1 1])))
                                       (Math/exp (+ (rand (- high low)) low))))
   :log-normal (fn [mean std & rest] ((if (empty? rest) identity (partial * (rand-nth [-1 1])))
                                      (Math/exp (dist/rand-normal mean std))))})

;;STD IS EFFECTIVELY HALF OF WHAT IT SHOULD BE
(defn calculate-inherit-weights
  [discount-factor inherit-depth std distribution num-cases std-to-scale]
  (let [offsets (reductions + 0 (map #(* std std-to-scale (Math/pow discount-factor %)) 
                                   (range inherit-depth)))
        offsets (let [m (math-tools/mean offsets)] (map #(- % m) offsets))
        parameters (map #(vector (/ (- (second %) (first %)) 2 std-to-scale) (- (math-tools/mean %))) 
                        (partition 2 1 offsets))]
    (map (partial apply dist/get-distribution distribution num-cases) parameters)))

(defn softmaxtest
  []
  (let [w (calculate-inherit-weights 0.5 10 20 :normal 3 1)]
    (print (math-tools/softmax (flatten w)))))

(defn weighted-lexicase
  "inherit-parameters is a list of [std, offset] pairs. "
  [pop argmap & {:keys [weights log discount-factor exponent inherit-depth std distribution std-to-scale]
                 :or {exponent [:uniform 1 1]
                      inherit-depth 0
                      discount-factor 0.5
                      distribution :normal
                      std-to-scale 2}}]
  (let [get-errors (if (:inherit-depth argmap) :inherited-errors #(vector (:errors %)))
        inherit-depth (if inherit-depth (inc inherit-depth) 1)
        pop (map rand-nth (vals (group-by get-errors pop)))
        num-cases (count (first (get-errors (first pop))))
        weights (math-tools/softmax
                 (or weights
                     (flatten (calculate-inherit-weights
                                     discount-factor inherit-depth std distribution num-cases std-to-scale))))
        transform (if log #(Math/log (max 0.1 %)) identity)
        weighted-error #(math-tools/generalized-dot 
                         (map transform (flatten (get-errors %))) 
                         weights
                         :pow (apply (exponent-sampler (first exponent)) (rest exponent)))]
    (apply min-key weighted-error pop)))

(defn epsilon-list
  "List of epsilons for each training case based on median absolute deviation of errors."
  [pop]
  (let [error-list (map :errors pop)
        length (count (:errors (first pop)))]
    (loop [epsilons [] i 0]
      (if (= i length)
        epsilons
        (recur (conj epsilons
                     (math-tools/median-absolute-deviation
                      (map #(nth % i) error-list)))
               (inc i))))))

(defn dominates
  [i j is-best errors epsilons]
  (not
   (or
    (and (some identity (map #(and %1 %2) (nth is-best i) (nth is-best j)))
         (some identity (map #(> %1 (+ %2 %3)) (nth errors i) (nth errors j) epsilons)))
    (some identity (map #(and (not %1) %2) (nth is-best i) (nth is-best j))))))

(defn dominates2 
  [i j is-best errors epsilons]
  (and (every? identity (map #(<= %1 %2) (nth errors i) (nth errors j)))
       (some identity (map #(< (+ %1 %3) %2) (nth errors i) (nth errors j) epsilons))))


#_(defn dominated-indices-test
  [pop argmap E epsilons errors is-best]
  (let [pop-size (count pop)]
    (loop [dominated #{}
           sorted-indices [721 628 856 466 399 179 178 400 757 878 168 158 408 409 539 410 540 894
                           427 927  91 925 923 552 630 607 423 124 903 416 541 896 424 940 725 467
                           342 336 661 319 768 312 771 310 651 649 778 300 298 293 503 787 798 214
                           675 836 829 239 674 633 821 820 248 819 251 670 270 245 950 348 972 572
                           996 954   9  69 447 437 599  47   2  41 579 951 961 463  20 977 100  50
                           881 592  27 799 988 154 286 573 884 892 334  75  93 337  64 475 715 897
                           942 918 703 261 926 578 585 604 440  59  33 520 735 201 818 813 726 199
                           259 202 688 244 390 386 223 835 231 469 233 215 827 826 238 838 574 246
                           588 250 392  70 796 638   6 769 315 359 767 491 761 760 330 453 331 333
                           756 339 655 659 997 343 344 656 332 811 458 652 748 510 272 751 639 481
                           752 792 789 772  16 364 363 645 295 297 777 304 361 683 445 850 635   0
                           615 544 404 120 700 882 709 710 616 407 127 960 129 567 535 898 136 159
                           413 156 716 618 851 962 968 717 433 891 893 975 912 608 110 561 978  73
                           190 858 560 860 559 941 937 601 185  86 933 146 429 866 676  90 867 556
                           438 870  98 435 554 101 593 916 184 145 596  35 590 993 501 919  80 464
                           648 830 115 812 955 831 455 206 833 917 895 523 995 175 358 224 861 222
                           389 764 324 839 762 702 519 428 597 708 296 810 477 807 268 142 514 888
                           538 252 804 969 351 277 133 930 724  24 886 162 640 644 732 723 825 957
                           947 405 730 170 367 636 397 476 355 460 697 431 712 718 686 729 754 425
                           385 360 357 441 755 685 707 832 846 242 518 235 834 221 118 909 841 842
                           845 634 122 123 194  14 859 529 868  30 131 872 137 141 165 151 913 576
                           610  31 658 759 338  42 329 490 970 966   7 305 959 583  13 502 507 280
                           273 952 511  38  79 558 924 915  12 994 619  51 421 976 216 512 335 260
                           320 847 517 740 169 138  39 478 318 454 516 149 415 989 219 684 815  22
                           232 479 484 581 678 580 626 564 492 494 998 506 665 669 472 528 627 465
                           486 532 534 617 549 462 457 584 212 203 204 207 209 218 823 253  25 814
                           61 271 793 791 790 785 783 264 193 879 164  65  72  74 945 939  97 922
                           104 113  29 117 119 901 153 155 887 163 294 299 808 775 303 375 743 758
                           345 340 763  17 354 398 317 701   3 695 773 774 854 380 992 112 382 890
                           378 384 625 276 899 322 106 347 679  23  63 569 637 371  21 352 366  67
                           314 401 744 806 589 256 369 747 350 907 157 144 986 795 485 551  66 290
                           446 509 555 920 575 871 436   8 990 981 779 614 182 852 653 372 474 387
                           391 719 439  54 935 985 914 837 682 999 328 562  28  10  53 198  44 524
                           236 766 301 931 548 302 266 788 281 211 687 570 139 196 677 711 908 307
                           189 402  18  37 249 987 187 801 965  84 114 862 602  55 568 285 452 844
                           776 183 728 900 150 172 722 284 487 681 257 237 586 470 125 376 316 191
                           802 171 828 180 176 111 121 258 946 262 526 542 152 403  58 220 864 622
                           971 480 956 396 108 690 148 770 160 325 365 229 750 228 321 265 287 736
                           720 713 840 279 704 418 824 974 929 606 533 632 984 521  52 613  71 664
                           99 623 921 482 504 #_0 1 848 849 197 195  45 192 103  46  48 973  49 857
                           964 188 963  56 958 186  57 855 967 205 200   #_0 853   4   5  11 234 230  15
                           991  19  26  32 227 226 983 225 217 213 982  34 980 979 210 208 843  36
                           40 863  43  60 181 865 936  85 934  87 135 134 132 130 932  88 128 928
                           902 938 126 905 240  89  92  94  95 910 911 116  96 109 107 105 904 140
                           143 147  62 102  68 869 177 949 174 948 873 874 875 876 877 173  76  77
                           880 944 167 883 166 943 885 161  78  81 889  82  83 953 906 283 822 667
                           666 663 662 660 483 657 488 489 654 493 495 496 650 497 498 647 668 473
                           471 671 443 694 693 692 691 689 444 448 646 449 451 680 456 459 461 468
                           673 672 450 500 643 642 547 609 550 605 553 603 557 600 611 598 565 595
                           594 566 591 571 577 587 563 696 612 545 641 505 508 513 515 522 525 631
                           546 629 530 531 624 621 620 536 537 543 527 442 698 699 289 784 291 782
                           781 780 292 306 786 308 311 313 765 323 326 327 341 346 309 349 288 282
                           243 247 254 817 816 255 263 809 582 267 805 274 803 800 275 278 797 794
                           269 241 353 362 393 394 395 406 411 412 414 714 727 417 420 422 706 705
                           426 430 432 434 419 356 388 731 753 368 749 370 373 746 745 374 383 742
                           377 739 738 737 379 734 733 381 741 499]
           d (read-string (str "[" (slurp "plexitest2.txt") "]"))]
            (println "dominated 853 " (dominated 853))
      (if (empty? sorted-indices)
        (do (println (filter #(not (dominated %)) (range pop-size)))
            (println "dominated 853 " (dominated 853))
            (println "1 dominates 853 " (dominates 1 853 is-best errors epsilons))
            (println "853 dominates 1 " (dominates 853 1 is-best errors epsilons))
            dominated)
        (do (println (first sorted-indices))
            (if (= (set (first d)) dominated) (println (dominated 853) ((set (first d)) 853)) (do (println (sort dominated)) (println (sort (first d))) (assert false)))
          (let [i (first sorted-indices)
              i-E (nth E i)
              to-compare (set (filter #(and (<= (nth E %) i-E) (not (= i %))) (range pop-size)))
              to-compare (set/difference to-compare dominated)
              dominated (set/union dominated (set (filter #(dominates i % is-best errors epsilons) to-compare)))]
          (recur
           dominated
           (filter #(not (dominated %)) (rest sorted-indices))
           (rest d))))))))


(defn dominated-indices
  [pop argmap E epsilons errors is-best]
  (let [pop-size (count pop)]
    (loop [dominated #{}
           sorted-indices (sort-by (partial nth E) (range pop-size))]
      (if (empty? sorted-indices)
        dominated
        (let [i (first sorted-indices)
              i-E (nth E i)
              to-compare (set (filter #(and (<= (nth E %) i-E) (not (= i %))) (range pop-size)))
              to-compare (set/difference to-compare dominated)
              dominated (set/union dominated (set (filter #(dominates2 i % is-best errors epsilons) to-compare)))]
          (recur
           dominated
           (filter #(not (dominated %)) (rest sorted-indices))))))))

;;Why didn't 892 get assimilated on step 466?

#_(defn dominated-indices
  [pop argmap E epsilons]
  (let [errors (map :errors pop)
        dominates (fn [i j] (and (every? identity (map #(<= (+ %1 %3) %2) (nth errors i) (nth errors j) epsilons))
                                 (some identity (map #(< (+ %1 %3) %2) (nth errors i) (nth errors j) epsilons))))]
    (println (for [i (range (count errors))] (dominates i 0)))
    (loop [dominated #{}
           indices (set (range (count pop)))]
      (if (empty? indices)
        dominated
        (let [i (first indices)]
          (if (dominated i)
            (recur dominated (rest indices))
            (let [e (nth E i)
                  unchecked (set/difference (set (filter #(>= e (nth E %)) (range (count pop)))) dominated)
                  dominated-indices (set (filter (partial dominates i) unchecked))]
              (recur (clojure.set/union dominated dominated-indices)
                     (clojure.set/difference indices dominated-indices #{i})))))))))

(defn w-commas
  [coll]
  (if (coll? coll)
    (apply str
           (concat ["["]
                   (drop-last (interleave (map w-commas coll) (repeat ", ")))
                   ["]"]))
    coll))

(defn assign-probabilities
  [pop argmap]
  (let [alpha (get argmap :alpha 1.)
        epsilons (if (= (:parent-selection argmap) :epsilon-plexicase) (epsilon-list pop) (map (constantly 0) pop))
        num-cases (count (:errors (first pop)))
        errors (map :errors pop)
        threshold (map (fn [idx eps]
                          (+ eps (apply min (map #(nth % idx) errors))))
                        (range num-cases)
                        epsilons)
        E (map (fn [e] (count (filter identity (map <= e threshold)))) errors)
        is-best (map (fn [e] (map <= e threshold)) errors)
        d (dominated-indices pop argmap E epsilons errors is-best)
        E (map-indexed #(if (d %1) 0 %2) E)
        normalize (fn [coll] (let [m (reduce + coll)] (map #(/ % m) coll)))
        h (for [j (range num-cases)]
            (normalize
             (for [i (range (count pop))]
               (if (<= (nth (nth errors i) j)
                      (nth threshold j))
                 (nth E i)
                 0))))
        p (normalize (map (fn [i] (Math/pow (math-tools/mean (map #(nth % i) h)) alpha)) (range (count pop))))]
    (mapv #(assoc %1 :probability %2) pop p)))

;;testing plexicase implementation
(defn plexicase-probabilities
  [errors alpha epsilon? nonzero-errors other-errors]
  (let [pop (map #(assoc {} :errors %) errors)
        argmap {:alpha alpha :parent-selection (if epsilon? :epsilon-plexicase :plexicase)}
        pop (assign-probabilities pop argmap)]
    (println)
    (let [nonzero (filter identity (map-indexed #(when (> (:probability %2) 0) %1) pop))
          nerrors (map #(:probability (nth pop %)) nonzero)]
      (println "errors:")
      ;; (pprint/pprint errors)
      (println "Python plexicase:")
      (println nonzero-errors)
      (println other-errors)
      (println "Clojure plexicase: ")
      (println (into [] nonzero))
      (println (into [] nerrors))
      (println)
      (println "Analysis:")
      (if (and (= (count nonzero) (count nonzero-errors)) (every? identity (map = nonzero nonzero-errors))) 
        (println "All nonzero indices match up")
        (println "Some incorrect nonzero indices"))
      (if (and (= (count nerrors) (count other-errors)) (every? identity (map #(> 1E-3 (math-tools/abs (- %1 %2))) nonzero nonzero-errors))) 
        (println "All probabilities match up")
        (println "Some incorrect probabilities")))))

(defn plexicase-test 
  []
  (let [[errors nonzero other-errors] [(read-string (slurp "plexitest.txt")) [] []]
        alpha 1 
        epsilon? true]
    (plexicase-probabilities errors alpha epsilon? nonzero other-errors)))

(defn reduction-probs
  [pop argmap]
  (assoc argmap :reductions (reductions + (map :probability pop))))

(defn plexicase
  [pop argmap]
  (let [red (:reductions argmap)
        r (rand)]
    (nth pop (first (filter #(> (nth red %) r) (range (count pop)))))))

(defn epsilon-lexicase-selection
  "Selects an individual from the population using epsilon-lexicase selection.
  Epsilon lexicase selection follows the same process as lexicase selection except,
  for a test case, only individuals with an error outside of a predefined epsilon are filtered."
  [pop argmap]
  (let [epsilons (:epsilons argmap)]
    (loop [survivors pop
           cases (shuffle (range (count (:errors (first pop)))))]
      (if (or (empty? cases)
              (empty? (rest survivors)))
        (rand-nth survivors)
        (let [min-err-for-case (apply min (map #(nth % (first cases))
                                               (map :errors survivors)))
              epsilon (nth epsilons (first cases))]
          (recur (filter #(<= (Math/abs (- (nth (:errors %)
                                                (first cases))
                                           min-err-for-case))
                              epsilon)
                         survivors)
                 (rest cases)))))))

(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)
    :epsilon-lexicase (epsilon-lexicase-selection pop argmap)
    :weighted-lexicase (apply weighted-lexicase pop argmap :inherit-depth (:inherit-depth argmap) (mapcat identity (:wlexi-parameters argmap)))
    :plexicase (plexicase pop argmap)
    :epsilon-plexicase (plexicase pop argmap)
    :truncation (truncation-selection pop argmap)))
