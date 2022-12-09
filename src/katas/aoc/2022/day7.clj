(ns katas.aoc.2022.day7
  (:require
   [clojure.test :refer :all]
   [clojure.tools.trace :refer :all]
   [katas.aoc.util :as util]
   [clojure.string :as str]))

(def initial-state
  {:filesystem {:name "/" :contents '() :type :dir}
   :current-dir []})

(defn add-fs-element-recurse-contents [fn-to-apply next-dir-name contents]
  (loop [remaining-fs-elements contents computed-fs-elements []]
    (cond
      ;; no more fs elements left
      (empty? remaining-fs-elements) computed-fs-elements
      ;; found subdirectory
      (= next-dir-name (:name (first remaining-fs-elements))) (recur (rest remaining-fs-elements) (conj computed-fs-elements (fn-to-apply (first remaining-fs-elements))))
      ;; keep other fs elements unchanged
      :else (recur (rest remaining-fs-elements) (conj computed-fs-elements (first remaining-fs-elements))))))

(defn add-fs-element [current-directory fs-element-to-add filesystem]
  (cond
    ;; in current-directory and in correct directory
    (and (= 1 (count current-directory)) (= (:name filesystem) (first current-directory))) (update filesystem :contents #(conj % fs-element-to-add))
    ;; ended up in wrong directory
    (= 1 (count current-directory)) (assert false "Ended up in wrong directory")
    ;; search for dir in directory contents
    :else (let [next-dir (second current-directory)
                rest-dir (rest current-directory)
                fn-for-subdir (partial add-fs-element rest-dir fs-element-to-add)]
            (update filesystem :contents (partial add-fs-element-recurse-contents fn-for-subdir next-dir)))))

(defn apply-cd-command [command state]
  (let [dir (subs command 5)]
    (if (= ".." dir)
      (-> state (update :current-dir #(-> % drop-last vec)))
      (-> state (update :current-dir #(conj % dir))))))

(defn add-directory-listing [directory-listing state]
  (let [directory-to-add {:name (subs directory-listing 4)
                          :contents '()
                          :type :dir}]
    (update state :filesystem (partial add-fs-element (:current-dir state) directory-to-add))))

(defn handle-file-listing [file-listing state]
  (let [[size filename] (str/split file-listing #" ")
        new-file {:name filename :size (Integer/parseInt size) :type :file}]
    (update state :filesystem (partial add-fs-element (:current-dir state) new-file))))

(defn apply-command [state command]
  (cond
    (str/starts-with? command "$ cd") (apply-cd-command command state)
    (str/starts-with? command "$ ls") state
    (str/starts-with? command "dir ") (add-directory-listing command state)
    :else (handle-file-listing command state)))

(defn compute-directory-size [filesystem]
  (if (= :file (:type filesystem))
    filesystem
    (let [files (filter #(= :file (:type %)) (:contents filesystem))
          file-sizes (->> files (map :size) (apply +))
          directories (filter #(= :dir (:type %)) (:contents filesystem))
          directory-sizes (->> directories (map compute-directory-size) (apply +))]
      (+ file-sizes directory-sizes))))

(defn add-dir-sizes-to-filesystem [filesystem]
  (if (= :file (:type filesystem))
    filesystem
    (-> filesystem
        (assoc :size (compute-directory-size filesystem))
        (update :contents #(map add-dir-sizes-to-filesystem %)))))


(deftrace get-dirs-for-predicate [pred filesystem]
  (let [sub-dirs-matching-pred (->> (:contents filesystem) (map (partial get-dirs-for-predicate pred)) flatten (filter (comp not empty?)))]
    (if (and (= :dir (:type filesystem))
             (pred filesystem))
      (conj sub-dirs-matching-pred filesystem)
      sub-dirs-matching-pred)))

(def get-dirs-under-100000 (partial get-dirs-for-predicate #(> 100000 (:size %))))

(defn solve-day-7-part-1 []
  (let [filesystem  (->> (util/load-file-from-resources-lines-as-string-coll! "aoc/2022/day7-input.txt")
                         (reduce apply-command initial-state)
                         :filesystem)]
    (->> filesystem add-dir-sizes-to-filesystem get-dirs-under-100000 (map :size) (apply +))))

(def total-space-on-disk 70000000)
(def space-needed-for-update 30000000)


(defn solve-day-7-part-2 []
  (let [filesystem  (->> (util/load-file-from-resources-lines-as-string-coll! "aoc/2022/day7-test-input.txt")
                         (reduce apply-command initial-state)
                         :filesystem
                         add-dir-sizes-to-filesystem)
        space-free (- total-space-on-disk (:size filesystem))
        space-needed (- space-needed-for-update space-free)
        find-deletion-folder-candidates-fn  (partial get-dirs-for-predicate #(>= (:size %) space-needed))]
    (->> filesystem find-deletion-folder-candidates-fn
         (map :size)
         (sort #(compare %1 %2))
         first
          )))


