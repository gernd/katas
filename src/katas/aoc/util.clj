(ns katas.aoc.util
  "Utility functions for Advent of Code"
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn load-file-from-resources-as-string! [filename]
  "Load the given resource file as string and removes trailing newlines"
  (-> filename
      io/resource
      slurp
      s/trim-newline))
