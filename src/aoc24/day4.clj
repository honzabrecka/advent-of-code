(ns aoc24.day4
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day4.txt")

(defn get-grid!
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (mapv vec)
         (doall))))

(defn go-dir
  [grid x y dx dy]
  (let [p (get-in grid [y x])]
    (lazy-seq (cons p (when p (go-dir grid (+ x dx) (+ y dy) dx dy))))))

(defn xmas
  [grid x y]
  (->> [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]
       (map (fn [[dx dy]] (take 4 (go-dir grid x y dx dy))))
       (filter #(= % [\X \M \A \S]))))

(defn solve-1
  []
  (let [grid (get-grid!)
        w (count (get grid 0))
        h (count grid)]
    (->> (for [x (range w)
               y (range h)]
           [x y])
         (filter (fn [[x y]] (= (get-in grid [y x]) \X)))
         (mapcat (fn [[x y]] (xmas grid x y)))
         count)))

(defn x-mas
  [grid x y]
  (let [r #{\S \M}
        f (fn [diffs]
             (into #{}
                   (map (fn [[dx dy]] (get-in grid [(+ y dy) (+ x dx)])))
                   diffs))]
    (and (= (f [[-1 -1] [1 1]]) r)
         (= (f [[1 -1] [-1 1]]) r))))

(defn solve-2
  []
  (let [grid (get-grid!)
        w (count (get grid 0))
        h (count grid)]
    (->> (for [x (range w)
               y (range h)]
           [x y])
         (filter (fn [[x y]] (= (get-in grid [y x]) \A)))
         (filter (fn [[x y]] (x-mas grid x y)))
         count)))
