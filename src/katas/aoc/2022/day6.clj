(ns katas.aoc.2022.day6
  (:require
   [clojure.test :refer :all]
   [clojure.tools.trace :refer :all]
   [katas.aoc.util :as util]
   [clojure.string :as str]))

(def initial-state
  {:last-chars (list)
   :position-for-first-marker nil
   :current-pos 0})

(defn update-char-fifo [fifo-size last-chars new-char]
  (if (< (count last-chars) fifo-size)
    (conj last-chars new-char)
    (-> last-chars
        (conj new-char)
        drop-last)))

(def update-start-of-packet-fifo (partial update-char-fifo 4))
(def update-start-of-message-fifo (partial update-char-fifo 14))

(defn x-chars-unique? [nr-of-chars chars]
  (and
   (= (count chars) nr-of-chars)
   (= (count chars) (count (set chars)))))

(def is-start-of-packet-marker? (partial x-chars-unique? 4))
(def is-start-of-message-marker? (partial x-chars-unique? 14))

(defn update-state [update-fifo-fn is-start-packet?-fn current-state next-char]
  (if (not (nil? (:position-for-first-marker current-state))) current-state
      (let [new-chars (update-fifo-fn (:last-chars current-state) next-char)
            new-position (inc (:current-pos current-state))]
        (cond-> current-state
          true (assoc :current-pos new-position)
          true (assoc :last-chars new-chars)
          (is-start-packet?-fn new-chars) (assoc :position-for-first-marker new-position)))))

(def update-state-start-of-packet-marker (partial update-state update-start-of-packet-fifo is-start-of-packet-marker?))
(def update-state-start-of-message-marker (partial update-state update-start-of-message-fifo is-start-of-message-marker?))

(comment defn update-state-start-of-packet-marker [current-state next-char]
  (if (not (nil? (:position-for-first-marker current-state))) current-state
      (let [new-chars (update-start-of-packet-fifo (:last-chars current-state) next-char)
            new-position (inc (:current-pos current-state))]
        (cond-> current-state
          true (assoc :current-pos new-position)
          true (assoc :last-chars new-chars)
          (is-start-of-packet-marker? new-chars) (assoc :position-for-first-marker new-position)))))

(defn compute-position-for-first-marker [update-state-fn datastream]
         (->> datastream
              (reduce update-state-fn initial-state)
              (:position-for-first-marker)))

(def compute-position-for-first-start-of-packet-marker (partial compute-position-for-first-marker update-state-start-of-packet-marker))
(def compute-position-for-first-start-of-message-marker (partial compute-position-for-first-marker update-state-start-of-message-marker))

(comment defn compute-position-for-first-start-of-packet-marker [datastream]
  (->> datastream
       (reduce update-state-start-of-packet-marker initial-state)
       (:position-for-first-marker)))

(defn solve-day-6-part-1 []
  (->> (util/load-file-from-resources-as-string! "aoc/2022/day6-input.txt")
       (compute-position-for-first-start-of-packet-marker)))

(defn solve-day-6-part-2 []
  (->> (util/load-file-from-resources-as-string! "aoc/2022/day6-input.txt")
       (compute-position-for-first-start-of-message-marker)))
