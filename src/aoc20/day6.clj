(ns aoc20.day6
  (:require [clojure.java.io :as io]))

(def input "./data/aoc20/day6.txt")

(def split-by-empty-line
  (comp (partition-by #{""})
        (remove #(= % [""]))))

(defn parse
  [x]
  (map #(into #{} %) x))

(defn sum-answers-with
  [f]
  (comp split-by-empty-line
        (map parse)
        (map #(apply f %))
        (map count)))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (transduce (sum-answers-with clojure.set/union) + (line-seq reader))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (transduce (sum-answers-with clojure.set/intersection) + (line-seq reader))))
