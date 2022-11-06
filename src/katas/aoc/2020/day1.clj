(ns katas.aoc.2020.day1)

(def expenses (hash-set 1721
                        979
                        366
                        299
                        675
                        1456))

(defn create-sum-record [first-number second-number]
  {:first first-number :second second-number :sum (+ first-number second-number)})

(defn create-sums [all-numbers number]
  (let [other-numbers (disj all-numbers number)
        create-record-fn (partial create-sum-record number)]
    (map create-record-fn other-numbers)))

(defn create-sums-for-expenses [expenses]
  (let [create-sum-record-fn (partial create-sums expenses)]
    (->> expenses
         (map create-sum-record-fn)
         flatten
         (filter #(= 2020 (:sum %)))
         first
         (#(* (:first %) (:second %))))))

(create-sums-for-expenses expenses)

