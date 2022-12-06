(ns katas.aoc.util
  "Utility functions for Advent of Code"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [clojure.tools.trace :refer :all]))

(defn load-file-from-resources-as-string!
  "Load the given resource file as string and removes trailing newlines"
  [filename]
  (-> filename
      io/resource
      slurp
      str/trim-newline))

(defn load-file-from-resources-lines-as-string-coll!
  "Loads the given resource file and returns a collection with an element for every line of the file "
  [filename]
  (-> filename
      load-file-from-resources-as-string!
      (str/split #"\n")))

(defn update-collection-parse-state [current-state line]
  (if (= "" line)
    (-> current-state
        (update :collections conj (:current-coll current-state))
        (update :current-coll empty))
    (update current-state :current-coll conj line)))

(defn string-collection-with-separator-lines->collection-of-string-colls
  "Can handle both string collections with and without a final empty string"
  [string-collection-with-separator-lines]
  (let  [normalized-collection (if (not= "" (last string-collection-with-separator-lines))
                                 (concat string-collection-with-separator-lines [""])
                                 string-collection-with-separator-lines)]
    (-> (reduce update-collection-parse-state {:collections '[] :current-coll '[]} normalized-collection)
        :collections
        seq))
  )

(deftest test-string-collection-with-separator-lines->collection-of-string-colls
  (testing "empty-collection results in collection containing an empty vector"
    (is (= '([])
           (string-collection-with-separator-lines->collection-of-string-colls '()))))
  (testing "Collection with one separator parsed into one collection"
    (is (=
         '(["23123" "999" "1"])
         (string-collection-with-separator-lines->collection-of-string-colls '("23123" "999" "1" "")))))
  (testing "Collection with two separator lines and a traling newline parsed into two collections"
    (is (=
         '(["a" "b" "c"] ["99" "20b" "dog"])
         (string-collection-with-separator-lines->collection-of-string-colls '("a" "b" "c" "" "99" "20b" "dog" "")))))
  (testing "Collection with two separator lines and no trailing newline parsed into two collections"
    (is (=
         '(["a" "b" "c"] ["99" "20b" "dog"])
         (string-collection-with-separator-lines->collection-of-string-colls '("a" "b" "c" "" "99" "20b" "dog"))))))

(defn load-file-with-blank-lines-separators-as-collections!
  "Loads the given resource file with several entries separated by blank lines"
  [filename]
  (-> filename
      io/resource
      slurp
      (str/split #"\n")
      string-collection-with-separator-lines->collection-of-string-colls))

