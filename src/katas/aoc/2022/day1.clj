(ns katas.aoc.2022.day
  (:require
   [clojure.test :refer :all]
   [clojure.tools.trace :refer :all]
   [katas.aoc.util :as util]))

(defn solve-part-1 []
  (->> (util/load-file-with-blank-lines-separators-as-collections! "aoc/2022/day1-input.txt")
       (map #(map (fn [s] (Integer/parseInt s)) %))
       (map #(apply + %))
       (apply max)))

(defn solve-part-2 []
  (->> (util/load-file-with-blank-lines-separators-as-collections! "aoc/2022/day1-input.txt")
       (map #(map (fn [s] (Integer/parseInt s)) %))
       (map #(apply + %))
       (sort #(compare %2 %1))
       (take 3)
       (apply +)))
