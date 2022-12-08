(ns aoc22.day8
  (:require [clojure.java.io :as io]))

(def input "./data/aoc22/day8.txt")

(defn parse-line
  [coll]
  (mapv #(-> % str parse-long) coll))

(defn get-grid!
  [input]
  (with-open [reader (io/reader input)]
    (let [grid (into [] (map parse-line) (line-seq reader))
          width (count (get grid 0))
          height (count grid)]
      (reduce (fn [acc [x y]]
                (assoc acc [x y] (get-in grid [y x])))
              {}
              (for [x (range width)
                    y (range height)]
                [x y])))))

(defn apply-delta
  [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn visible-in-direction?
  [grid p d]
  (let [value (get grid p)]
    (loop [p# (apply-delta p d)]
      (let [value# (get grid p#)]
        (cond
          (nil? value#) true
          (>= value# value) false
          :else (recur (apply-delta p# d)))))))

(defn visible?
  [grid [x y]]
  (or (visible-in-direction? grid [x y] [0 1])
      (visible-in-direction? grid [x y] [0 -1])
      (visible-in-direction? grid [x y] [1 0])
      (visible-in-direction? grid [x y] [-1 0])))

(defn solve-1
  []
  (let [grid (get-grid! input)]
    (->> (keys grid)
         (filter #(visible? grid %))
         count)))

(defn count-visible-in-direction
  [grid p d]
  (let [value (get grid p)]
    (loop [p# (apply-delta p d)
           score 1]
      (let [value# (get grid p#)]
        (cond
          (nil? value#) (dec score)
          (>= value# value) score
          :else (recur (apply-delta p# d) (inc score)))))))

(defn count-visible
  [grid [x y]]
  (* (count-visible-in-direction grid [x y] [0 1])
     (count-visible-in-direction grid [x y] [0 -1])
     (count-visible-in-direction grid [x y] [1 0])
     (count-visible-in-direction grid [x y] [-1 0])))

(defn solve-2
  []
  (let [grid (get-grid! input)]
    (->> (keys grid)
         (map #(count-visible grid %))
         (apply max))))
