(ns aoc15.day5
  (:require [clojure.java.io :as io]))

(def input "./data/aoc15/day5.txt")

(defn nice?
  [s]
  (and
    (>= (count (re-seq #"[aeiou]" s)) 3)
    (re-find #"(.)\1{1,}" s)
    (nil? (re-find #"(ab|cd|pq|xy)" s))))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (filter nice?)
         (count))))

(defn nice-v2?
  [s]
  (and
    (re-find #"(..)(.*?)\1{1,}" s)
    (re-find #"(.).\1{1,}" s)))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (filter nice-v2?)
         (count))))
