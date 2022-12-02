(ns katas.aoc.2020.day3
  (:require
   [clojure.test :refer :all]
   [clojure.tools.trace :refer :all]))

(def test-landscape-input
  ["..##......."
   "#...#...#.."
   ".#....#..#."
   "..#.#...#.#"
   ".#...##..#."
   "..#.##....."
   ".#.#.#....#"
   ".#........#"
   "#.##...#..."
   "#...##....#"
   ".#..#...#.#"])

(defn geology-char->geology-symbol [geology-char]
  (case geology-char
    \. :square
    \# :tree))

(defn create-landscape-coordinate
  "creates a new landscape coordinate from the given y/x coordinate and the given input char"
  [y-coordinate x-coordinate geology-input-char]
  {:x x-coordinate :y y-coordinate :geology (geology-char->geology-symbol geology-input-char)})

(deftest test-create-landscape-coordinate
  (testing "Creating a landscape coordinate works for square"
    (is (= {:x 2 :y 8 :geology :square}
           (create-landscape-coordinate 8 2 \.)))))

(defn create-landscape-coordinate-row [y-coordinate landscape-row-input-string]
  (let [create-coordinate-fn (partial create-landscape-coordinate y-coordinate)]
    (->> landscape-row-input-string
         (map-indexed create-coordinate-fn))))

(deftest test-create-landscape-coordinate-row
  (testing "creating a row works"
    (is (=
         (list
          {:x 0 :y 2 :geology :square}
          {:x 1 :y 2 :geology :square}
          {:x 2 :y 2 :geology :tree})
         (create-landscape-coordinate-row 2 "..#")))))

(defn landscape-input->landscape-coordinates
  "Converts raw landscape input data into a set of coordinates with their symbols"
  [landscape-input-data]
  (loop [coordinates (list) remaining-input landscape-input-data current-y-position 0]
    (cond
      (empty? remaining-input) (flatten coordinates)
      :else (let
             [current-input (first remaining-input)
              coordinate-row-for-input (create-landscape-coordinate-row current-y-position current-input)
              all-coordinates (conj coordinates coordinate-row-for-input)]
              (recur all-coordinates (rest remaining-input) (inc current-y-position))))))

(def test-landscape-coordinates (landscape-input->landscape-coordinates test-landscape-input))

(deftest test-landscape-input->landscape-coordinates
  (testing "empty input results in empty set"
    (is (= () (landscape-input->landscape-coordinates [])))))

(defn compute-max-y-pos
  "Computes the maximum y position for the given landscape coordinates"
  [landscape-coordinates]
  (->> landscape-coordinates
       (map :y)
       (apply max)))

(deftrace update-position
  "Computes the next position of the slope"
  [current-position]
  (-> current-position
      (update :x #(+ 3 %))
      (update :y inc)))

(deftrace get-geology-for-position
  "Computes the geology for the given position and coordinates"
  [coordinates x-pos y-pos]
  ;; TOOD case where max x or max y is 0 -> currently leads to an arithmetic exception
  ;; -> extract internal function
  (let [max-x (->> coordinates (map :x) (apply max))
        normalized-x-pos (mod x-pos max-x)]
    (->> coordinates
         (filter #(and (= normalized-x-pos (:x %))
                       (= y-pos (:y %))))
         first
         :geology)))

(deftest test-get-geology-for-position
  (testing "Correct geology returned for coordinate (0,0)"
    (is (= :tree
           (get-geology-for-position
            '({:x 0 :y 0 :geology :tree}
              {:x 1 :y 0 :geology :square}
              {:x 0 :y 1 :geology :square}
              {:x 1 :y 1 :geology :square})
            0 0))))
  (testing "Correct geology returned for coordinate (1,1)"
    (is (= :square
           (get-geology-for-position
            '({:x 0 :y 0 :geology :tree}
              {:x 1 :y 0 :geology :square}
              {:x 0 :y 1 :geology :square}
              {:x 1 :y 1 :geology :square})
            1 1))))
  (testing "Correct geology returned for coordinate including wraparound"
    (is (= :square
           (get-geology-for-position
            '({:x 0 :y 0 :geology :tree}
              {:x 1 :y 0 :geology :square}
              {:x 0 :y 1 :geology :square}
              {:x 1 :y 1 :geology :square})
            3 1)))))

(defn create-initial-state [landscape-coordinates]
  (let [max-y-coordinate (compute-max-y-pos landscape-coordinates)]
    {:pos {:x 0 :y 0}
     :landscape-coordinates landscape-coordinates
     :tree-counter 0
     :max-y-coordinate max-y-coordinate
     :has-next-state (> max-y-coordinate 0)}))

(def test-state (-> test-landscape-input landscape-input->landscape-coordinates create-initial-state))

(defn compute-new-tree-counter [current-tree-counter coordinates current-position]
  (let [current-geology (get-geology-for-position coordinates (:x current-position) (:y current-position))]
    (if (= :tree current-geology) (inc current-tree-counter)
        current-tree-counter)))

(deftest test-compute-new-tree-counter
  (testing "Tree counter stays the same for coordinates containing only a square"
    (is (= 2
           (compute-new-tree-counter 2 '({:x 0 :y 0 :geology :square}) {:x 0 :y 0})))))

(defn compute-next-state
  "Computes the next state for the Toboggan simulation"
  [current-state]
  (let [current-position (:pos current-state)
        next-pos (update-position current-position)
        has-next-state (not= (:max-y-coordinate current-state) (:y current-position))
        new-tree-counter (compute-new-tree-counter (:tree-counter current-state) (:landscape-coordinates current-state) current-position)]
    (-> current-state
        (assoc :pos next-pos)
        (assoc :has-next-state has-next-state)
        (assoc :tree-counter new-tree-counter))))

(def test-state-sequence (iterate compute-next-state test-state))
