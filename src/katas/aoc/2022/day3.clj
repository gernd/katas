(ns katas.aoc.2022.day3
  (:require
   [clojure.set :as sets]
   [clojure.test :refer :all]
   [clojure.tools.trace :refer :all]
   [katas.aoc.util :as util]))

(defn string->set-of-chars [string]
  (->> string
       seq
       (into #{})))

(defn parse-rucksack-line-part-1 [rucksack-line]
  (let [length (.length rucksack-line)
        half-size (/ length 2)
        first-half (subs rucksack-line 0 half-size)
        second-half (subs rucksack-line half-size)]
    {:first-compartment (string->set-of-chars first-half)
     :second-compartment (string->set-of-chars second-half)}))

(defn generate-priorities [start-char-code end-char-code start-priority]
  (zipmap
   (->> (range start-char-code (+ end-char-code 1)) (map char))
   (iterate inc start-priority)))

(defn generate-all-priorities []
  (let [lowercase-priorities (generate-priorities 97 122 1)
        uppercase-priorities (generate-priorities 65 90 27)]
    (merge lowercase-priorities uppercase-priorities)))

(def rucksack-prios (generate-all-priorities))

(defn get-score [rucksack-item]
  (get rucksack-prios rucksack-item))

(defn compute-rucksack-score [rucksack]
  (-> (sets/intersection (:first-compartment rucksack) (:second-compartment rucksack))
      first
      get-score))

(defn solve-day-3-part-1 []
  (->> "aoc/2022/day3-input.txt"
       (util/load-file-from-resources-lines-as-string-coll!)
       (map parse-rucksack-line-part-1)
       (map compute-rucksack-score)
       (apply +)))

(defn parse-rucksack-line-part-2 [rucksack-line]
  {:rucksack-content (string->set-of-chars rucksack-line)})

(defn find-badge [rucksack-group]
  (->> rucksack-group
       (map :rucksack-content)
       (apply sets/intersection)))

(defn solve-day-3-part-2 []
  (->> "aoc/2022/day3-input.txt"
       (util/load-file-from-resources-lines-as-string-coll!)
       (map parse-rucksack-line-part-2)
       (partition 3)
       (map find-badge)
       (map first)
       (map get-score)
       (apply +)))


