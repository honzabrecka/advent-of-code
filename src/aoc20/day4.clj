(ns aoc20.day4
  (:require [clojure.java.io :as io]
            [aoc20.shared :as shared]))

(def input "./data/aoc20/day4.txt")

(defn parse
  [record]
  (into {} (comp (filter (complement #(= "" %)))
                 (mapcat #(clojure.string/split % #" "))
                 (map (fn [x]
                        (let [[key val] (clojure.string/split x #":")]
                          [(keyword key) val]))))
        record))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [lines (line-seq reader)
          required #{:byr :iyr :eyr :hgt :hcl :ecl :pid}]
      (->> lines
           (shared/split-by #(= "" %))
           (map parse)
           (map keys)
           (filter #(= (clojure.set/difference required (set %)) #{}))
           (count)))))
