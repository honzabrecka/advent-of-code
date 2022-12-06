(ns aoc22.day4
  (:require [clojure.java.io :as io]))

(def input "./data/aoc22/day4.txt")

(defn parse-line
  [s]
  (->> (re-matches #"^([\d]+)-([\d]+),([\d]+)-([\d]+)$" s)
       (drop 1)
       (map parse-long)))

(defn realize-assignments
  [[a b c d]]
  [(set (range a (inc b)))
   (set (range c (inc d)))])

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (map realize-assignments)
         (filter (fn [[a b]]
                   (or (clojure.set/subset? a b)
                       (clojure.set/subset? b a))))
         count)))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (map realize-assignments)
         (map #(apply clojure.set/intersection %))
         (remove empty?)
         count)))
