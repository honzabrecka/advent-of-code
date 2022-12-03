(ns aoc15.day2
  (:require [clojure.java.io :as io]))

(def input "./data/aoc15/day2.txt")

(defn parse
  [row]
  (->> (clojure.string/split row #"x")
       (map #(Integer/parseInt %))))

(defn area
  [xs]
  (let [[w h l] (sort xs)]
    (+ (* w h)
       (* 2 w h)
       (* 2 w l)
       (* 2 l h))))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (transduce (comp (map parse)
                     (map area))
               + (line-seq reader))))
