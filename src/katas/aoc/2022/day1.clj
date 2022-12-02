(ns katas.aoc.2022.day
  (:require
   [clojure.test :refer :all]
   [clojure.tools.trace :refer :all]
   [katas.aoc.util :as util]))

(->> (util/load-file-with-blank-lines-separators-as-collections! "aoc/2022/day1-test-input.txt")
     (map #(map (fn [s] (Integer/parseInt s)) %))
     (map #(apply + %))
     (apply max)
     )
