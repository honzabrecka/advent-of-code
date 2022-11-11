(ns aoc17.day4
  (:require [clojure.java.io :as io]))

(def input "./data/aoc17/day4.txt")

(defn get-input!
  [input]
  (with-open [reader (io/reader input)]
    (into []
          (map #(map set (clojure.string/split % #"\s")))
          (line-seq reader))))

(defn solve-1
  []
  (->> (get-input! input)
       (filter (fn [xs]
              (= (count xs)
                 (count (set xs)))))
       (count)))

(defn solve-2
  []
  (->> (get-input! input)
       (filter (fn [xs]
                 (= (count xs)
                    (count (for [x xs
                                 y xs
                                 :when (= x y)]
                             x)))))
       (count)))
