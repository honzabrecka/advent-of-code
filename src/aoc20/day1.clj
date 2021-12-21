(ns aoc20.day1
  (:require [clojure.math.combinatorics :as combo]
            [clojure.java.io :as io]))

(def input "./data/aoc20/day1.txt")

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [ns (map #(Integer/parseInt %) (line-seq reader))
          [matching] (->> (combo/combinations ns 2)
                          (filter #(= 2020 (apply + %))))]
      (apply * matching))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [ns (map #(Integer/parseInt %) (line-seq reader))
          [matching] (->> (combo/combinations ns 3)
                          (filter #(= 2020 (apply + %))))]
      (apply * matching))))
