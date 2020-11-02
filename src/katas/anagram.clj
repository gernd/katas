(ns katas.anagram)

(defn are-anagrams?
  "Check if two given words are anagrams"
  [word1 word2]
  (= (frequencies word1) (frequencies word2)))