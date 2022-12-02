(ns katas.aoc.2022.day2
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]
   [clojure.tools.trace :refer :all]
   [katas.aoc.util :as util]))

(defn opponent-move-string->opponent-move-symbol [strategy-string]
  (case strategy-string
    "A" :rock
    "B" :paper
    "C" :scissors))

(defn strategy-string->strategy-symbol [opponent-move-string]
  (case opponent-move-string
    "X" :rock
    "Y" :paper
    "Z" :scissors))

(defn line->game-description-part-1 [line]
  (-> line
      (str/split #" ")
      ((fn [[opponent-move-string strategy-string]]
         (hash-map :strategy (strategy-string->strategy-symbol strategy-string) :opponent-move (opponent-move-string->opponent-move-symbol opponent-move-string))))))

(defn compute-strategy-score [strategy]
  (case strategy
    :rock 1
    :paper 2
    :scissors 3))

(defn compute-result-score [{:keys [:strategy :opponent-move]}]
  (cond
  ;; winning situations
    (or
     (and (= :rock strategy) (= :scissors opponent-move))
     (and (= :paper strategy) (= :rock opponent-move))
     (and (= :scissors strategy) (= :paper opponent-move))) 6
  ;;draw
    (= strategy opponent-move) 3
;; losing situations
    :else 0))

(defn compute-score [game-description]
  (+ (compute-result-score game-description) (compute-strategy-score (:strategy game-description))))

(defn solve-part-1 []
  (->>  "aoc/2022/day2-input.txt"
        util/load-file-from-resources-lines-as-string-coll!
        (map line->game-description-part-1)
        (map compute-score)
        (apply +)))

(defn desired-result-string->desired-result-symbol [desired-result-string]
  (case desired-result-string
    "X" :lose
    "Y" :draw
    "Z" :win))

(defn line->game-description-part-2 [line]
  (-> line
      (str/split #" ")
      ((fn [[opponent-move-string desired-result-string]]
         (hash-map :desired-result (desired-result-string->desired-result-symbol desired-result-string) :opponent-move (opponent-move-string->opponent-move-symbol opponent-move-string))))))

(defn desired-result->score [designated-result]
  (case designated-result
    :lose 0
    :draw 3
    :win 6))

(defn compute-strategy-for-desired-result [opponent-move designated-result]
  (let [mapping {:rock {:lose :scissors
                        :draw :rock
                        :win :paper}
                 :paper {:lose :rock
                         :draw :paper
                         :win :scissors}
                 :scissors {:lose :paper
                            :draw :scissors
                            :win :rock}}]
    (get-in mapping [opponent-move designated-result])))

(defn compute-result-part-2 [{:keys [:desired-result :opponent-move]}]
  (let [strategy-for-result (compute-strategy-for-desired-result opponent-move desired-result)]
    (+ (desired-result->score desired-result)
       (compute-strategy-score strategy-for-result))))

(defn solve-part-2 []
  (->>  "aoc/2022/day2-input.txt"
        util/load-file-from-resources-lines-as-string-coll!
        (map line->game-description-part-2)
        (map compute-result-part-2)
        (apply +)))
