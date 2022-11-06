(ns katas.aoc.2015.day1
  (:require [katas.aoc.util :as util]))

(defn symbol->fn
  "Returns the corresponding function for the given symbol"
  [sym]
  (condp = sym
    \( inc
    \) dec))

(def test-string "((()((())(())")

(defn compute-floor [paranthesis]
  (let [floor-functions (map symbol->fn paranthesis)]
    (reduce (fn [current-level function] (function current-level))
            0
            floor-functions)))

(comment compute-floor test-string)

(defn solve-day-1 []
  (->
   (util/load-file-from-resources-as-string! "aoc/2015/day1-input.txt")
   compute-floor))




