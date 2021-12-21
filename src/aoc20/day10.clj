(ns aoc20.day10
  (:require [clojure.java.io :as io]))

(def input "./data/aoc20/day10.txt")

(defn parse
  [s]
  (Integer/parseInt s))

(defn diffs
  [xs]
  (->> (concat [0] xs)
       (sort)
       (partition 2 1)
       (map (fn [[a b]] (- b a)))))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [xs (map parse (line-seq reader))
          fqs (->> xs diffs frequencies)]
      (* (get fqs 1)
         (inc (get fqs 3))))))

(def count-all-distinct-ways
  (memoize
    (fn [[x & xs]]
      (cond
        (nil? x) 1
        (> x 3) 0
        :else (+ (count-all-distinct-ways xs)
                 (if xs
                   (let [[y & ys] xs]
                     (count-all-distinct-ways (concat [(+ y x)] ys)))
                   0))))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [xs (map parse (line-seq reader))]
      (->> xs diffs count-all-distinct-ways))))
