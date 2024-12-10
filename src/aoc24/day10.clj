(ns aoc24.day10
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day10.txt")

(defn parse-line
  [line]
  (->> (clojure.string/split line #"")
       (mapv parse-long)))

(defn get-grid!
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (into []))))

(defn starts
  [grid]
  (for [x (range (count (first grid)))
        y (range (count grid))
        :let [v (get-in grid [y x])]
        :when (= v 0)]
    [x y]))

(def neighbours [[-1 0] [1 0] [0 -1] [0 1]])

(defn all-paths
  ([grid start]
   (all-paths grid 1 [start] start))
  ([grid i path [x y]]
   (if (= i 10)
     [path]
     (->> neighbours
          (map (fn [[dx dy]]
                 [(+ x dx) (+ y dy)]))
          (filter (fn [[x y]]
                    (= (get-in grid [y x]) i)))
          (mapcat (fn [pos]
                    (all-paths grid (inc i) (conj path pos) pos)))))))

(defn trailheads
  [paths]
  (into #{} (map (juxt first last)) paths))

(defn solve-1
  []
  (let [grid (get-grid!)]
    (->> (starts grid)
         (map (fn [start] (all-paths grid start)))
         (map trailheads)
         (map count)
         (reduce +))))

(defn solve-2
  []
  (let [grid (get-grid!)]
    (->> (starts grid)
         (map (fn [start] (all-paths grid start)))
         (map count)
         (reduce +))))
