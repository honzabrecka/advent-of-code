(ns aoc24.day8
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day8.txt")

(defn get-patterns!
  []
  (let [grid (with-open [reader (io/reader input)]
               (into [] (map vec) (line-seq reader)))
        w (count (first grid))
        h (count grid)]
    [w h (->> (for [y (range h)
                    x (range w)]
                [x y])
              (filter (fn [[x y]] (not= \. (get-in grid [y x]))))
              (group-by (fn [[x y]] (get-in grid [y x]))))]))

(defn inside?
  [w h [x y]]
  (and (>= x 0) (>= y 0) (< x w) (< y h)))

(defn compute-diff
  [[x1 y1] [x2 y2]]
  [(- x1 x2)
   (- y1 y2)])

(defn anti-nodes-diffs
  [nodes]
  (for [a nodes
        b nodes
        :when (not= a b)]
    [(compute-diff a b) a]))

(defn apply-diff
  [[dx dy] [x y]]
  [(+ x dx) (+ y dy)])

#_(defn anti-node-seq
  [[dx dy] [x y]]
  (let [x' (+ x dx)
        y' (+ y dy)]
    (lazy-seq (cons [x' y'] (lazy-seq (anti-node-seq [dx dy] [x' y']))))))

#_(iterate #(apply-diff [1 0] %) [0 0])

#_(defn anti-nodes
  [nodes]
  (for [a nodes
        b nodes
        :when (not= a b)
        :let [[dx dy] (compute-diff a b)
              [x y] a
              x' (+ x dx)
              y' (+ y dy)]]
    [x' y']))

#_(defn solve-1
  []
  (let [[w h patterns] (get-patterns!)]
    (->> (vals patterns)
         (mapcat anti-nodes)
         (filter #(inside? w h %))
         (into #{})
         (count))))

(defn solve-1
  []
  (let [[w h patterns] (get-patterns!)]
    (->> (vals patterns)
         (mapcat (fn [nodes]
                   (->> (anti-nodes-diffs nodes)
                        (map (fn [[diff start]]
                               (apply-diff diff start))))))
         (filter #(inside? w h %))
         (into #{})
         (count))))

(defn solve-2
  []
  (let [[w h patterns] (get-patterns!)]
    (->> (vals patterns)
         (mapcat (fn [nodes]
                   (->> (anti-nodes-diffs nodes)
                        (mapcat (fn [[diff start]]
                                  (->> (iterate #(apply-diff diff %) start)
                                       (take-while #(inside? w h %))
                                       (concat [start])))))))
         (into #{})
         (count))))
