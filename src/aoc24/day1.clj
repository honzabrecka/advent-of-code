(ns aoc24.day1
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day1.txt")

(defn parse-line
  [line]
  (->> (clojure.string/split line #"[ ]+")
       (map parse-long)))

(defn rotate
  [matrix]
  (apply map vector matrix))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [[a b] (->> (line-seq reader)
                (map parse-line)
                     rotate)]
      (->> (map vector (sort a) (sort b))
           (map (fn [[a b]] (abs (- a b))))
           (reduce +)))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [[a b] (->> (line-seq reader)
                     (map parse-line)
                     rotate)
          fqs (frequencies b)]
      (->> a
           (map (fn [n]
                  (* n (get fqs n 0))))
           (reduce +)))))
