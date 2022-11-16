(ns katas.aoc.2016.day1
  (:require [katas.aoc.util :as util]
            [clojure.test :refer :all]
            [clojure.string :as s]
            [clojure.tools.trace :refer :all]))

(defn initialize-position []
  {:x 0 :y 0})

(defn initialize-state []
  {:position (initialize-position)
   :direction :north})

(defn parse-instruction [instruction-string]
  (let [command-char (first instruction-string)
        distance-string (-> instruction-string rest s/join)]
    {:command (condp = command-char
                \R :right
                \L :left)
     :distance (Integer/parseInt distance-string)}))

(deftest test-parse-instruction
  (testing "correctly parses a rightround instruction"
    (is (= {:command :right :distance 7} (parse-instruction "R7"))))
  (testing "correctly parses a leftround instruction"
    (is (= {:command :left :distance 100} (parse-instruction "L100")))))

(def direction-instruction-table
  {:north {:left :west :right :east}
   :east {:left :north :right :south}})

(defn compute-new-direction [command current-direction]
  (get-in direction-instruction-table [current-direction command])
  )

(deftest test-compute-new-direction
  (testing "Computing new direction works for turning leftward when facing North"
    (is (= :west (compute-new-direction :left :north)))
    (testing "Computing new direction works for turning rightward when facing East"
      (is (= :south (compute-new-direction :right :east))))))
