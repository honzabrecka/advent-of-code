(ns aoc20.day2
  (:require [clojure.java.io :as io]))

(def input "./data/aoc20/day2.txt")

(defn parse
  [s]
  (let [[_ a b letter password] (re-matches #"^([0-9]+)\-([0-9]+) ([a-z]): ([a-z]+)$" s)]
    [(Integer/parseInt a) (Integer/parseInt b) (.charAt letter 0) password]))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [lines (map parse (line-seq reader))]
      (->> lines
           (filter (fn [[min max letter password]]
                     (let [fqs (frequencies password)
                           count (get fqs letter 0)]
                       (and (>= count min)
                            (<= count max)))))
           (count)))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [lines (map parse (line-seq reader))]
      (->> lines
           (filter (fn [[a b letter password]]
                     (let [chars (into [] password)
                           matches (into #{} (map #(get chars (dec %))) [a b])]
                       (and (= (count matches) 2)
                            (contains? matches letter)))))
           (count)))))
