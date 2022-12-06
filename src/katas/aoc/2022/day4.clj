(ns katas.aoc.2022.day4
  (:require
   [clojure.test :refer :all]
   [clojure.tools.trace :refer :all]
   [katas.aoc.util :as util]
   [clojure.string :as str]))

(defn section-range-line->section-range [section-range-string]
  (let [[min-section max-section] (str/split section-range-string #"-")]
    [(Integer/parseInt min-section) (Integer/parseInt max-section)]))

(defn section-line->sections [section-line]
  (let [[first-elve-sections second-elve-sections] (str/split section-line #",")]
    {:first-elve-sections (section-range-line->section-range first-elve-sections)
     :second-elve-sections  (section-range-line->section-range second-elve-sections)}))

(defn section-contained?
  "Checks if the second section is fully contained in the first one"
  [[section-1-min section-1-max] [section-2-min section-2-max]]
  (and (<= section-1-min section-2-min) (<= section-2-max section-1-max)))

(defn full-containment-present? [sections]
  (or
   (section-contained? (:first-elve-sections sections) (:second-elve-sections sections))
   (section-contained? (:second-elve-sections sections) (:first-elve-sections sections))))

(defn solve-day4-part-1 []
  (->> (util/load-file-from-resources-lines-as-string-coll! "aoc/2022/day4-input.txt")
       (map section-line->sections)
       (filter full-containment-present?)
       count))

(defn sections-overlap?  [sections]
  (let [[section-1-min section-1-max] (:first-elve-sections sections)
        [section-2-min section-2-max] (:second-elve-sections sections)]
  (or
   (and (<= section-1-min section-2-min) (<= section-2-min section-1-max))
   (and (<= section-1-min section-2-max) (<= section-2-max section-1-max)))))

(defn solve-day4-part-2 []
  (->> (util/load-file-from-resources-lines-as-string-coll! "aoc/2022/day4-input.txt")
       (map section-line->sections)
       (filter #(or (sections-overlap? %) (full-containment-present? %) ))
       count))
