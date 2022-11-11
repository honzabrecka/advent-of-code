(ns aoc17.day5
  (:require [clojure.java.io :as io]))

(def input "./data/aoc17/day5.txt")

(defn get-input!
  [input]
  (with-open [reader (io/reader input)]
    (into []
          (map parse-long)
          (line-seq reader))))

(defn solve-1
  []
  (loop [jumps (get-input! input)
         position 0
         steps 1]
    (let [next-position (+ position (get jumps position))]
      (if (>= next-position (count jumps))
        steps
        (recur (update jumps position inc)
               next-position
               (inc steps))))))

(defn solve-2
  []
  (loop [jumps (get-input! input)
         position 0
         steps 1]
    (let [next-position (+ position (get jumps position))]
      (if (>= next-position (count jumps))
        steps
        (recur (update jumps position (fn [offset]
                                        (if (>= offset 3)
                                          (dec offset)
                                          (inc offset))))
               next-position
               (inc steps))))))
