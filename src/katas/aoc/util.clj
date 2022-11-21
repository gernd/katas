(ns katas.aoc.util
  "Utility functions for Advent of Code"
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.string :as str]))

(defn load-file-from-resources-as-string!
  "Load the given resource file as string and removes trailing newlines"
  [filename]
  (-> filename
      io/resource
      slurp
      s/trim-newline))

(defn load-file-from-resources-lines-as-string-coll!
  "Loads the given resource file and returns a collection with an element for every line of the file "
  [filename]
  (-> filename
      load-file-from-resources-as-string!
      (str/split #"\n")
      ))
