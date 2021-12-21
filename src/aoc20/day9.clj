(ns aoc20.day9
  (:require [clojure.math.combinatorics :as combo]
            [clojure.java.io :as io]))

(def input "./data/aoc20/day9.txt")

(defn parse
  [s]
  (Long/parseLong s))

(defn find-invalid
  [preamble lines]
  (->> (into [] (comp (map (fn [xs]
                             [(last xs) (butlast xs)]))
                      (map (fn [[sum xs]]
                             [sum (combo/combinations xs 2)]))
                      (filter (fn [[sum combinations]]
                                (not-any? #(= (reduce + %) sum) combinations)))
                      (take 1)
                      (map first))
             (partition (inc preamble) 1 lines))
       first))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (find-invalid 25 (map parse (line-seq reader)))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [lines (map parse (line-seq reader))
          invalid-from-1 (find-invalid 25 lines)
          contiguous-set (->> (range 2 (count lines))
                              (pmap (fn [x]
                                      (into [] (comp
                                                 (filter #(= (reduce + %)
                                                             invalid-from-1))
                                                 (take 1))
                                            (partition x 1 lines))))
                              (remove #(= (count %) 0))
                              (flatten)
                              (sort))]
      (+ (first contiguous-set)
         (last contiguous-set)))))
