(ns katas.aoc.2022.day8
  (:require
   [clojure.test :refer :all]
   [clojure.tools.trace :refer :all]
   [katas.aoc.util :as util]
   [clojure.string :as str]))

(defn create-tree [y x height]
  {:x x :y y :height (-> height str Integer/parseInt)})

(defn row->trees [y row]
  (->> row (map-indexed (partial create-tree y))))

(defn parse-trees [tree-input-lines]
  (-> (for [y (range (count tree-input-lines))]
        (row->trees y (nth tree-input-lines y)))
      flatten))

(defn is-at-edge? [max-x max-y {:keys [x y]}]
  (or (= x 0) (= x max-x) (= y 0) (= y max-y)))

(defn is-visible-horizontally? [trees {:keys [x y height]}]
  (let [left-half (filter #(and (< (:x %) x) (= y (:y %))) trees)
        right-half (filter #(and (> (:x %) x) (= y (:y %))) trees)]
    (or
     (every? #(< (:height %) height) left-half)
     (every? #(< (:height %) height) right-half))))

(defn is-visible-vertically? [trees {:keys [x y height]}]
  (let [upper-half (filter #(and (< (:y %) y) (= x (:x %))) trees)
        lower-half (filter #(and (> (:y %) y) (= x (:x %))) trees)]
    (or
     (every? #(< (:height %) height) upper-half)
     (every? #(< (:height %) height) lower-half))))

(defn is-visible-from-outside? [trees tree]
  (or (is-visible-horizontally? trees tree) (is-visible-vertically? trees tree)))

(defn add-visible-flag [trees tree]
  (let [max-x (->> trees (map :x) (apply max))
        max-y (->> trees (map :y) (apply max))]
    (assoc tree :is-visible (or (is-at-edge? max-x max-y tree)
                                (is-visible-from-outside? trees tree)))))

(defn solve-day8-part-1 []
  (let [trees (-> (util/load-file-from-resources-lines-as-string-coll! "aoc/2022/day8-input.txt") parse-trees)
        add-visible-flag-fn (partial add-visible-flag trees)]
    (->> trees
         (map add-visible-flag-fn)
         (filter #(:is-visible %))
         count)))

(defn compute-scenic-view-for-direction [tree-height trees-in-direction]
  (cond
    (empty? trees-in-direction) 0
    (<= tree-height (:height (first trees-in-direction))) 1
    :else (+ 1 (compute-scenic-view-for-direction tree-height (rest trees-in-direction)))))

(defn compute-scenic-view [trees  {:keys [x y height]}]
  (let [compute-scenic-view-for-direction-fn (partial compute-scenic-view-for-direction height)
        ;; coordinates are sorted in ascending order
        trees-in-directions [;; down
                             (->> trees (filter #(and (= x (:x %)) (> (:y %) y))))
                            ;; up
                             (->> trees (filter #(and (= x (:x %)) (< (:y %) y))) reverse)
                            ;; left
                             (->> trees (filter #(and (= y (:y %)) (< (:x %) x))) reverse)
                            ;; right
                             (->> trees (filter #(and (= y (:y %)) (> (:x %) x))))]]
    (->> trees-in-directions
         (map compute-scenic-view-for-direction-fn)
         (apply *))))

(defn solve-day8-part-2 []
  (let [trees (-> (util/load-file-from-resources-lines-as-string-coll! "aoc/2022/day8-input.txt") parse-trees)
        compute-scenic-view-fn (partial compute-scenic-view trees)]
    (->> trees
         (map compute-scenic-view-fn)
         (apply max))))
