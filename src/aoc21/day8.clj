(ns aoc21.day8
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def input "./data/aoc21/day8.txt")

(defn parse-line
  [s]
  (let [[input output] (clojure.string/split s #" \| ")
        parse-digit (fn [s] (set (clojure.string/split s #"")))
        parse-digits (fn [s] (mapv parse-digit (clojure.string/split s #" ")))]
    [(parse-digits input)
     (parse-digits output)]))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (mapcat second)
         (filter #(contains? #{2 4 3 7} (count %)))
         (count))))

(def positions
  {[0 1 2 4 5 6]   0
   [2 5]           1
   [0 2 3 4 6]     2
   [0 2 3 5 6]     3
   [1 2 3 5]       4
   [0 1 3 5 6]     5
   [0 1 3 4 5 6]   6
   [0 2 5]         7
   [0 1 2 3 4 5 6] 8
   [0 1 2 3 5 6]   9})

(def ps (memoize (fn []
                   (->> (combo/permutations ["a" "b" "c" "d" "e" "f" "g"])
                        (map vec)
                        (map (fn [p] (update-keys positions #(set (map p %)))))))))

(defn compute
  [[input output]]
  (let [[match] (filter (fn [m] (every? #(get m %) input)) (ps))]
    (->> output
         (map match)
         (reduce str)
         parse-long)))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (pmap compute)
         (reduce +))))
