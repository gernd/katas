(ns katas.aoc.2015.day1
  (:require [katas.aoc.util :as util]
            [clojure.tools.trace :refer :all]))

;; part 1
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

;; part 2
(defn build-initial-floor-state [parantheses]
  {:paranthesis parantheses
   :current-pos 0
   :floor 0})

(defn compute-next-floor [current-floor paranthesis]
  (let [floor-mod-function (symbol->fn paranthesis)]
    (floor-mod-function current-floor)))

(defn compute-next-floor-state [current-state]
  (let [current-paranthesis (first (:paranthesis current-state))
        current-pos (inc (:current-pos current-state))
        current-floor (compute-next-floor (:floor current-state) current-paranthesis)]
    {:paranthesis (rest (:paranthesis current-state))
     :current-pos current-pos
     :floor current-floor}))

(defn compute-position-for-basement [parantheses]
  (let [floor-states (iterate compute-next-floor-state (build-initial-floor-state parantheses))]
    (->> floor-states
         (filter #(= -1 (:floor %)))
         first
         :current-pos)))

(defn solve-day-1-part-2 []
  (->
   (util/load-file-from-resources-as-string! "aoc/2015/day1-input.txt")
   compute-position-for-basement))



