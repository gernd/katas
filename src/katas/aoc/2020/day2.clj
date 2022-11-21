(ns katas.aoc.2020.day2
  (:require [katas.aoc.util :as util]
            [clojure.tools.trace :refer :all]
            [clojure.string :as s]
            [clojure.test :refer :all]))

(def first-password-line "1-3 a: abcde")
(def second-password-line "1-3 b: cdefg")

(defn string->char [string]
  (.charAt string 0))

(defn parse-password-spec-part-1
  "Parse a password spec string into a password specification"
  [spec-string]
  (let [[occurrence-string char-string] (s/split spec-string #" ")
        [min max] (s/split occurrence-string #"-")]
    {:min (Integer/parseInt min) :max (Integer/parseInt max) :char (string->char char-string)}))

(deftest test-parse-password-spec-part-1
  (testing "test a password spec"
    (is (= {:min 10 :max 42 :char \d} (parse-password-spec-part-1 "10-42 d"))))
  (testing "test another password spec"
    (is (= {:min 4 :max 10 :char \z} (parse-password-spec-part-1 "4-10 z")))))

(defn parse-password-line
  "Parses a passwort line into password spec and the actual password"
  [spec-parse-fn line]
  (let [[password-spec-string password]  (s/split line #":")]
    {:spec (spec-parse-fn password-spec-string) :password (s/triml password)}))

(def parse-password-line-part-1 (partial parse-password-line parse-password-spec-part-1))

(deftest test-password-line-part-1
  (testing "parsing a password line"
    (is (= {:spec {:min 1 :max 3 :char \a} :password "abcde"}
           (parse-password-line-part-1 "1-3 a: abcde")))))

(deftrace validate-line [password-line]
  (let [{:keys [spec password]} (parse-password-line-part-1 password-line)
        char-frequencies-in-password (->> password frequencies (merge-with + {(:char spec) 0}))
        char-occurence-in-password (get char-frequencies-in-password (:char spec))]
    (and (<= (:min spec) char-occurence-in-password)
         (>= (:max spec) char-occurence-in-password))))

(defn solve-part-1 []
  (->> (util/load-file-from-resources-lines-as-string-coll! "aoc/2020/day2-input.txt")
       (map validate-line)
       (filter true?)
       count))
