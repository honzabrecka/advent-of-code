(ns aoc22.day3
  (:require [clojure.java.io :as io]))

(def input "./data/aoc22/day3.txt")

(def lowercase-priority
  (into {}
        (map vector (map char (range 97 123))
                    (range 1 27))))

(def uppercase-priority
  (into {}
        (map vector (map char (range 65 91))
                    (range 27 53))))

(def combined-priorities
  (merge lowercase-priority
         uppercase-priority))

(defn parse-line-1
  [s]
  (->> (split-at (/ (count s) 2) (vec s))
       (map set)))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line-1)
         (mapcat #(apply clojure.set/intersection %))
         (map combined-priorities))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map set)
         (partition 3)
         (mapcat #(apply clojure.set/intersection %))
         (map combined-priorities)
         (reduce +))))
