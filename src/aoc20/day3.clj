(ns aoc20.day3
  (:require [clojure.java.io :as io]))

(def input "./data/aoc20/day3.txt")

(defn parse
  [s]
  (into [] (map #(if (= \. %) :empty :tree)) s))

(defn walk
  [right down lines]
  (->> lines
       (take-nth down)
       (keep-indexed (fn [i x] [x i]))
       (map (fn [[x i]]
              (let [j (* i right)
                    c (count x)]
                (get x (mod j c)))))
       (filter #(= :tree %))
       (count)))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [lines (map parse (line-seq reader))]
      (walk 3 1 lines))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [lines (map parse (line-seq reader))]
      (*
        (walk 1 1 lines)
        (walk 3 1 lines)
        (walk 5 1 lines)
        (walk 7 1 lines)
        (walk 1 2 lines)))))
