(ns katas.aoc.2022.day9
  (:require
   [clojure.test :refer :all]
   [clojure.tools.trace :refer :all]
   [katas.aoc.util :as util]
   [clojure.string :as str]
   [clojure.set :as s]))

(def initial-state {:tail-positions #{}
                    :current-positions {:head-position {:x 0 :y 0} :tail-position {:x 0 :y 0}}})

(deftrace update-state-for-tail [state]
  (let [current-positions (:current-positions state)
        {:keys [head-position tail-position]} current-positions]
    (cond
      ;; move follow horizontally
      ;; TODO 
      (= (:y head-position) (:y tail-position)) (update-in state [:tail-position :x] #(+ 1 %))

      ;; move follow vertically
      ;; TODO
      (= (:x head-position) (:x tail-position)) (update-in state [:tail-position :x] #(+ 1 %)))
    state))

(defn compute-next-positions [current-position head-movement]
  (let [update-state-for-head-fn #(update % :head-position (:update-head-position-fn head-movement))
        compute-next-pos-fn (comp update-state-for-tail update-state-for-head-fn)]
    (take (+ 1 (:nr-of-steps head-movement)) (iterate compute-next-pos-fn current-position))))

(defn compute-next-state [current-state head-movement]
  (let [next-positions (compute-next-positions (:current-positions current-state) head-movement)
        latest-positions (last next-positions)
        new-tail-positions (s/union (:tail-position current-state) (set (map :tail-position next-positions)))]
    (-> current-state
        (assoc :current-positions latest-positions)
        (assoc :tail-positions new-tail-positions))))

(defn head-movement-input->head-movement-description [movement-input]
  (let [[command nr-of-steps-str] (str/split movement-input #" ")
        nr-of-steps-int (Integer/parseInt nr-of-steps-str)
        update-position-fn (case command
                             "R" (fn [position] (update position :x inc))
                             "L" (fn [position] (update position :x dec))
                             "U" (fn [position] (update position :y inc))
                             "D" (fn [position] (update position :y dec)))]
    {:nr-of-steps nr-of-steps-int :update-head-position-fn update-position-fn}))

(defn solve-day-9-part-1 []
  (let [movement-descriptions  (->> (util/load-file-from-resources-lines-as-string-coll! "aoc/2022/day9-test-input.txt")
                                    (map head-movement-input->head-movement-description))]
    (reduce  compute-next-state initial-state movement-descriptions)))
