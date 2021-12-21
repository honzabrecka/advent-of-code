(ns aoc20.day5
  (:require [clojure.java.io :as io]))

(def input "./data/aoc20/day5.txt")

(defn parse
  [row]
  (->> (into [] row)
       (split-with #(contains? #{\F \B} %))))

(defn half
  [min max]
  (let [h (/ (- (inc max) min) 2)
        e (+ min h)]
    [[min (dec e)]
     [e max]]))

(defn walk
  [min max [step & steps]]
  (let [fl #{\F \L}]
    (if (= (- max min) 1)
      (if (contains? fl step) min max)
      (let [[a b] (half min max)
            [min max] (if (contains? fl step) a b)]
        (recur min max steps)))))

(defn sorted-ids
  []
  (with-open [reader (io/reader input)]
    (let [lines (line-seq reader)]
      (->> lines
           (map parse)
           (map (fn [[rs cs]]
                  (let [row (walk 0 127 rs)
                        col (walk 0 7 cs)]
                    (+ (* row 8) col))))
           (sort)))))

(defn solve-1
  []
  (last (sorted-ids)))

(defn solve-2
  []
  (let [ids (sorted-ids)]
    (clojure.set/difference
      (set (range (first ids)
                  (inc (last ids))))
      (set ids))))
