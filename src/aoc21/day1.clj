(ns aoc21.day1
  (:require [clojure.java.io :as io]))

(def input "./data/aoc21/day1.txt.txt")

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-long)
         (partition 2 1)
         (filter (fn [[a b]] (> b a)))
         (count))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-long)
         (partition 3 1)
         (map #(reduce + %))
         (partition 2 1)
         (filter (fn [[a b]] (> b a)))
         (count))))
