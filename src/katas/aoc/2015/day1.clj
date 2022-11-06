(ns katas.aoc.2015.day1)

(defn symbol->fn
  "Returns the corresponding function for the given symbol"
  [sym]
  (condp = sym
    \( inc
    \) dec))

(def test-string "((()((())(())")

(def test-string-fns (map symbol->fn test-string))

(reduce (fn [current-level function] (function current-level))
        0
        test-string-fns
        )



