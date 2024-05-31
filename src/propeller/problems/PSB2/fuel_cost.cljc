(ns propeller.problems.PSB2.fuel-cost
  "FUEL COST from PSB2

Given a vector of positive integers, divide
each by 3, round the result down to the nearest integer, and
subtract 2. Return the sum of all of the new integers in the
vector

Source: https://arxiv.org/pdf/2106.06086.pdf"
  {:doc/format :markdown}
  (:require [psb2.core :as psb2]
            [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.utils :as utils]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.push.state :as state]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            [propeller.tools.maxbandit :as maxbandit]
            [propeller.tools.gesmr :as gesmr]
            [clojure.pprint :as pprint]
            #?(:cljs [cljs.reader :refer [read-string]])))

(def train-and-test-data "Data taken from https://zenodo.org/record/5084812" (psb2/fetch-examples "data" "fuel-cost" 200 2000))

; Random integer between -100 and 100 (from smallest)
(defn random-int "Random integer between -100 and 100" [] (- (rand-int 201) 100))

(def instructions
  "Stack-specific instructions, input instructions, close, and constants"
  (utils/not-lazy
    (concat
      ;;; stack-specific instructions
      (get-stack-instructions #{:exec :integer :boolean :vector_integer :print})
      ;;; input instructions
      (list :in1)
      ;;; close
      (list 'close)
      ;;; ERCs (constants)
      (list random-int 0 1 2 3))))


(def train-data
  (map #(vector [(get % :input1)] (get % :output1)) (:train train-and-test-data)))

(def test-data
  (map #(vector [(get % :input1)] (get % :output1)) (:test train-and-test-data)))


(def error-function
  (gp/default-error-function :out-stacks :integer
                             :error (fn [correct-output output]
                                      (if (= output :no-stack-item)
                                        1000000
                                        (math/abs (- correct-output output))))))


(def umad-logprobs 
  [0.0, 0.4997757875794058, 0.5767368287155339, 0.7774075241776854, 0.9529722985087608, 0.9863087187763524, 0.9863087187763524, 0.9863087187763524, 0.9863087187763524, 0.9822019368236985, 0.9572006346182818, 0.9358778651494601, 0.9272198024063458, 0.8918178753554296, 0.8644189011673156, 0.8644189011673156, 0.8551165085050014, 0.8504326591925757, 0.8504326591925757, 0.8644189011673156, 0.8644189011673156, 0.8736355562722391, 0.8873031950009027, 0.8918178753554296, 0.9228624970373902, 0.9228624970373902, 0.9228624970373902, 0.9358778651494601, 0.9529722985087608, 0.9656040454146613, 0.9656040454146613, 0.9656040454146613, 0.9656040454146613, 0.9572006346182818, 0.9487260076273092, 0.9358778651494601, 0.9315582040049435, 0.9184861224375913, 0.9184861224375913, 0.9184861224375913, 0.9272198024063458, 0.9272198024063458, 0.9272198024063458, 0.9272198024063458, 0.9358778651494601, 0.944461608840852, 0.9572006346182818, 0.9614111671546253, 0.9572006346182818, 0.9572006346182818, 0.9572006346182818, 0.9572006346182818, 0.9487260076273092, 0.9315582040049435, 0.9228624970373902, 0.9096754927554365, 0.9096754927554365, 0.9140905109645532, 0.9096754927554365, 0.9140905109645532, 0.9140905109645532, 0.9184861224375913, 0.9184861224375913, 0.9228624970373902, 0.9272198024063458, 0.9272198024063458, 0.9007865453381898, 0.8963122649432691, 0.8963122649432691, 0.9007865453381898, 0.9007865453381898, 0.8963122649432691, 0.8918178753554296, 0.8873031950009027, 0.8782122232996512, 0.8782122232996512, 0.8736355562722391, 0.86903784702361, 0.86903784702361, 0.8551165085050014, 0.8504326591925757, 0.8504326591925757, 0.8457267681551626, 0.8457267681551626, 0.8644189011673156, 0.86903784702361, 0.86903784702361, 0.8782122232996512, 0.9315582040049435, 0.9401789470488504, 0.9697794168251415, 1.0574849972442468, 1.0688703194693723, 1.0801274739940068, 1.1022685998712207, 1.1095413592002998, 1.1095413592002998, 1.0912593143628513, 1.0912593143628513, 1.0650895966294662, 1.030404038641576, 0.9944720294155127, 0.978078219639837, 0.978078219639837, 0.978078219639837, 0.9944720294155127, 0.9944720294155127, 1.0145934326149337, 1.0650895966294662, 1.1555671815955826, 1.1929229681393512, 1.209452270090562, 1.2448827070700625, 1.2667928167058067, 1.2821541018672944, 1.2821541018672944, 1.2821541018672944, 1.2667928167058067, 1.2448827070700625, 1.2353436840233032, 1.2448827070700625, 1.2667928167058067, 1.276037874849858, 1.288233147943676, 1.323951230545755, 1.3556085504164468, 1.366876275262789, 1.4107189131269662, 1.4107189131269662, 1.3999371375236773, 1.3999371375236773, 1.3890378470656417, 1.3780184518160308, 1.335579268540875, 1.3151406008636002, 1.3121863889661691, 1.335579268540875, 1.3442124156855773, 1.3556085504164468, 1.3835433277480007, 1.3835433277480007, 1.394502341537721, 1.394502341537721, 1.4319398686098515, 1.4905553714443007, 1.5150064673084644, 1.543579839752521, 1.55987822148564, 1.5667827368321845, 1.5690736885787402, 1.5713594038595966, 1.5736399065583218, 1.5736399065583218, 1.580450375560848, 1.6225385364797988, 1.6268675475693843, 1.6482358819750829, 1.7136989227585104, 1.7136989227585104, 1.7657839828247948, 1.8726471283671557, 1.8827312474337816, 1.9156289509408406, 2.0642163340827704, 2.19722457733622, 2.3777129530485137, 2.3777129530485137, 2.4485390056171257, 2.490386115552626, 2.5244090927642473, 2.5581639200614097, 2.591639849257799, 2.6684484629871843, 2.6910568110147777, 2.6767976575981063, 2.6623322359697488, 2.6460968580987867, 2.6460968580987867, 2.6184380424125235, 2.593284586470677, 2.5858618719779107, 2.577549272158545, 2.577549272158545, 2.570849079588725, 2.5683248880928184, 2.5899924024272005, 2.575878427993727, 2.550474860393588, 2.550474860393588, 2.486736476465077, 2.3612685948221364, 2.330755969960742, 2.2633643798407643, 2.2261074514850057, 2.1737522211510774, 2.1737522211510774, 2.20815364786841, 2.2225423853205095, 2.3777129530485137, 2.579217329259242, 2.631967894644771, 2.7376445658537607, 2.7376445658537607, 2.7656968962018604, 2.882403588246988, 2.9824577423452956, 3.0109997456847797, 3.041377781043952, 3.1539562787688853, 3.2686074609414746, 3.2814894175961875, 3.3404124905930335, 3.401929714173831, 3.403027208339232, 3.435404137063691, 3.438231993313667, 3.460226246988757, 3.574011122621341, 3.574011122621341, 3.6121052087172103, 3.83838829404994, 3.886197883659406, 4.024959149022553, 4.117159239988272, 4.291963653225038, 4.408994865674817, 4.505774749798741, 4.548541689894046, 4.584518846989651, 4.641109952133004, 4.641109952133004, 4.779585109700994, 4.855458446913685, 4.937710054191584, 5.169734463460276, 5.409337659582226, 5.409337659582226, 5.549204353061375, 5.618208625366906, 5.631251168181874, 5.631251168181874, 5.669767237094501, 5.762569598058397, 5.794921462961808, 6.031688560675457, 6.100811611405337, 6.121456027784605, 6.145764352393195, 6.198590387197848, 6.198590387197848, 6.229897215340376, 6.260190611802081, 6.2750930854093365, 6.371254875475804, 6.396838075931317, 6.433770107532839, 6.468466297121033, 6.473263040456725, 6.473263040456725, 6.471785646853252, 6.471785646853252, 6.489989552785735, 6.506703494740753, 6.522448261811581, 6.585413504704638, 6.600453419660704, 6.60477733162935, 6.605833101658792, 6.602304553261892, 6.600034947079497, 6.596455839831698, 6.566796067853009, 6.566796067853009, 6.552978062261884, 6.488085362994086, 6.425812529364933, 6.377100653945803, 6.261009225871284, 6.224710740600001, 6.199036945358305, 6.062404240255983, 6.048566488046895, 6.014655094076097, 5.962788047830372, 5.873324304803758, 5.842333300106166, 5.727312972066395, 5.6200424641841344, 5.4997769615363215, 5.036738507854382, 4.836281906951479, 4.758504205908794, 4.620545877371191, 4.5314171804120775, 4.343519952254258, 4.307474777035324, 4.307474777035324, 4.021616308430545, 3.863155915223059, 3.743149084047994, 3.4229373682985615, 3.300303878508429, 3.1539562787688853, 3.0044850646635863, 2.6523128843297927, 2.6272135275274975, 2.564103691449194, 2.3156857170397442, 2.1118768174329903, 2.020018121209035, 1.942765492375842, 1.8365297031391608, 1.8365297031391608, 1.7695363324433453, 1.711716761554519, 1.6524552975177915, 1.580450375560848, 1.580450375560848, 1.5736399065583218, 1.557566081727261, 1.557566081727261, 1.545924506711775, 1.5365126725294278, 1.5341458075191623, 1.5125880864441834, 1.4680262583842723, 1.4319398686098515, 1.4187296368730449, 1.3297821508565484, 1.2698840092754793, 1.1794999478072103, 1.1521009736190964, 1.061294525660915, 0.8736355562722391])

(defn sample-umad
  []
  (let [idx (maxbandit/gumbel-max umad-logprobs)
        e (math/exp (- (* 10 (/ (maxbandit/sample-uniform idx (inc idx)) (count umad-logprobs))) 10))]
    {:e e}))

#_(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (gp/gp
    (merge
      {:instructions            instructions
       :error-function          error-function
       :training-data           train-data
       :testing-data            test-data
       :max-generations         300
       :population-size         1000
       :max-initial-plushy-size 250
       :step-limit              2000
       :parent-selection        :lexicase
       :tournament-size         5
       :umad-rate               0.2
       :variation               {:umad 1.0 :crossover 0.0}
       :elitism                 false}
      (apply hash-map (map #(if (string? %) (read-string %) %) args)))))

;; (def basis (maxbandit/make-random-basis 3 (count instructions)))
;; (pprint/pprint {:basis basis
;;                 :instructions (mapv str instructions)})

(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (gesmr/gp
   (merge
    {;;:mapper mapv
    ;;  :downsample?                 true ; wether to use downsampling
    ;;  :ds-function                 :case-maxmin ; :case-rand, case-maxmin, case-maxmin-auto
    ;;  :downsample-rate             0.05 ; proportion of data used in downsample
    ;;  :ds-parent-rate              0.01 ; proportion of parents used to evaluate case distances
    ;;  :ds-parent-gens              10 ; generations between computation of parent distances
     :instructions            instructions
     :error-function          error-function
     :training-data           train-data
     :testing-data            test-data
     :max-generations         300
     :population-size         [1000 1000]
     :max-initial-plushy-size 250
     :step-limit              2000
     :parent-selection        :lexicase
     :tournament-size         5
     :update-freq 100
     :report-interval 1
     :transformations [maxbandit/tile-to-parameters-umad maxbandit/parameters-to-tile-umad]
    ;;  :transformations [#(maxbandit/tile-to-parameters-random % :basis basis :low [-5 -5 -5] :high [4.9999 4.9999 4.9999] :sigma [0.5 0.5 0.5]) maxbandit/parameters-to-tile-random]
     :bandit-parameters {:num-bandits 5
                         :l [-10]
                         :r [0]
                         :to [[0 0.03 0.06 0.09 0.12 0.15]] #_[[0 0.03 0.06 0.09 0.12 0.15 0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39 0.42 0.45 0.48]]
                         :ta [[0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39]] #_[[0.51 0.54 0.57 0.6 0.63 0.66 0.69 0.72 0.75 0.78 0.81 0.84 0.87 0.9]]
                         :num-codings 20
                         :d [1]
                         :acc [0.03]
                         :lr (into [] (map (fn [lr] {:method :momentum :lr (math/pow 10 (- (/ lr 9) 4)) :dampening 1 :momentum-lr 0.1})) (range 10))  
                         #_[{:method :momentum
                               :momentum-lr 0.1
                               :lr 0.0001
                                                        ;;  :update-freq (int-array (apply concat (map #(repeat % (* 10 %)) (range 30))))
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
                         :AO true
                         :xi* [0.01]
                         :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0.01)))]
                         :maximize? false
                         :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                                                                          ;;  :dampening 1
                                                                          ;;  :momentum-lr [0.03]
                                                  ;;  :max-n (int-array (apply concat (map #(repeat % (* 10 %)) (range 30))))
                                                                          ;;  :legacy true
                         :max-n 100
                         :sigma 3}
     :reward-transformation :log-diff
    ;;  :parameter-override {:e 0.1}
    ;;  :parameter-analysis [{:e 0.01} {:e 0.03} {:e 0.1} {:e 0.3} {:e 1} {:e 3}]
     :umad-rate               0.1
     :variation               {:umad 1 :crossover 0}
     :elitism                 false}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))



#_{:num-bandits 1
   :l [-5]
   :r [5]
   :to [[0 0.03 0.06 0.09 0.12 0.15]] #_[[0 0.03 0.06 0.09 0.12 0.15 0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39 0.42 0.45 0.48]]
   :ta [[0.18 0.21 0.24 0.27 0.3 0.33 0.36 0.39]] #_[[0.51 0.54 0.57 0.6 0.63 0.66 0.69 0.72 0.75 0.78 0.81 0.84 0.87 0.9]]
   :num-codings 20
   :d [1]
   :acc [0.03]
   :lr [{:method :momentum
         :momentum-lr 0.1
         :lr 0.001
                                ;;  :update-freq (int-array (apply concat (map #(repeat % (* 10 %)) (range 30))))
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
   :AO true
   :xi* [0.01]
   :epsilon #_[1] [(double-array (concat (map #(/ (- 5. %) 5) (range 5)) (repeat 295 0)))]
   :maximize? false
   :temperature [(double-array (concat (map #(* 3/5 (inc %)) (range 5)) (repeat 295 3)))] #_[(double-array (concat (map #(* 1 (inc %)) (range 15)) (repeat 285 15)))]
                                                  ;;  :dampening 1
                                                  ;;  :momentum-lr [0.03]
                          ;;  :max-n (int-array (apply concat (map #(repeat % (* 10 %)) (range 30))))
                                                  ;;  :legacy true
   :max-n 100
   :sigma 3}


#_(defn -main
    "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
    [& args]
    (gesmr/gp
     (merge
      {#_:mapper #_mapv
       :instructions            instructions
       :error-function          error-function
       :training-data           train-data
       :testing-data            test-data
       :max-generations         300
       :population-size         [1000 1000]
       :max-initial-plushy-size 100
       :step-limit              200
       :parent-selection        :lexicase
       :tournament-size         5
       :update-freq 100
       :report-interval 1
       :generations-per-update 1
       :reward-transformation :log-diff
       :n-rates 100
       :rate-eta 0.1
       :rate-sigma 1.3
       :custom-report maxbandit/custom-report
       :umad-rate               0.1
       :variation               {:umad 1 :crossover 0}
       :elitism                 false}
      (apply hash-map (map #(if (string? %) (read-string %) %) args)))))

