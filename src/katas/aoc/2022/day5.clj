(ns katas.aoc.2022.day5
  (:require
   [clojure.test :refer :all]
   [clojure.tools.trace :refer :all]
   [katas.aoc.util :as util]
   [clojure.string :as str]))

(defn get-crate-from-input [position crates-line]
  (let [position-index (if (= 1 position) 1 (+ 1 (* 4 (- position 1))))
        char-at-position (get crates-line position-index)]
    (if (= \space char-at-position) nil char-at-position)))

(deftest test-get-crate-from-input
  (testing "getting the first crate works for an input containing only one crate"
    (is (= \A (get-crate-from-input 1 "[A]"))))
  (testing "getting the first crate works for an input containing two crates"
    (is (= \B (get-crate-from-input 1 "[B] [Z]"))))
  (testing "getting the second crate works for an input containing three crates"
    (is (= \D (get-crate-from-input 2 "[B] [D] [J]")))))

(defn compute-number-of-crate-stacks [crate-stack-description-line]
  (->> crate-stack-description-line
       str/trimr
       (map identity)
       last
       str
       Integer/parseInt))

(defn build-inititial-crate-stacks [nr-of-crate-stacks]
  (take nr-of-crate-stacks (repeatedly vector)))

(defn update-crate-stacks [stacks crate-input]
  (map-indexed (fn [index crate-contents]
                 (let [position (+ index 1)
                       crate (get-crate-from-input position crate-input)]
                   (if (nil? crate) crate-contents (conj crate-contents crate)))) stacks))

(defn crate-stacks-input->crate-stacks [crates-input]
  (let [crate-stack-description-line (last crates-input)
        crate-contents (-> crates-input drop-last reverse) ;; reverse cranes contents for correct ordering after parsing
        number-of-crates-stacks (compute-number-of-crate-stacks crate-stack-description-line)
        initial-crate-stacks (build-inititial-crate-stacks number-of-crates-stacks)]
    (reduce update-crate-stacks initial-crate-stacks crate-contents)))

(defn crane-command-input->crane-command [crane-command-input]
  (let [splitted-input (str/split crane-command-input #" ")]
    {:amount (-> splitted-input (get 1) Integer/parseInt)
     :from  (-> splitted-input (get 3) Integer/parseInt)
     :to  (-> splitted-input (get 5) Integer/parseInt)}))

(defn apply-crane-command [transform-fn crane-stacks {:keys [amount from to]}]
  (let [index-from (- from 1)
        index-to (- to 1)
        cranes-stack-vector (vec crane-stacks)
        from-cranes-stack (nth cranes-stack-vector index-from)
        to-cranes-stack (nth cranes-stack-vector index-to)
        cranes-to-transport (->> from-cranes-stack
                                 (take-last amount)
                                 transform-fn)
        new-from-stack-state (drop-last amount from-cranes-stack)
        new-to-stack-state (concat to-cranes-stack cranes-to-transport)]
    (-> cranes-stack-vector
        (assoc index-from new-from-stack-state)
        (assoc index-to new-to-stack-state))))


(def apply-command-for-crane-9000 (partial apply-crane-command reverse))
(def apply-command-for-crane-9001 (partial apply-crane-command identity))

(defn solve-day-5-part-1 []
  (let [[crates-input crane-commands-inputs] (util/load-file-with-blank-lines-separators-as-collections! "aoc/2022/day5-input.txt")
        initial-crate-stacks (crate-stacks-input->crate-stacks crates-input)
        crane-commands (map crane-command-input->crane-command crane-commands-inputs)]
    (->> (reduce apply-command-for-crane-9000 initial-crate-stacks crane-commands)
    (map last)
    (apply str)
    )))

