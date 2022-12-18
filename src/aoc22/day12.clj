(ns aoc22.day12
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]))

(def input "./data/aoc22/day12.txt")

(defn get-grid!
  [input]
  (with-open [reader (io/reader input)]
    (into []
          (map vec)
          (line-seq reader))))

(def dirs
  [[0 1] [0 -1] [1 0] [-1 0]])

(defn move
  [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn get-in-grid
  [grid]
  (fn [[x y]]
    (get-in grid [y x])))

(defn find-start
  [grid]
  (->> (for [x (range (inc (count (get grid 0))))
             y (range (inc (count grid)))]
         [x y])
       (filter (fn [[x y]] (= (get-in grid [y x]) \S)))
       first))

(defn find-end
  [grid]
  (->> (for [x (range (inc (count (get grid 0))))
             y (range (inc (count grid)))]
         [x y])
       (filter (fn [[x y]] (= (get-in grid [y x]) \E)))
       first))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.

  Returns a map from nodes to their distance from start."
  [start f]
  (loop [q (priority-map start 0)
         r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (update-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn neighbours
  [get-in-grid [x y]]
  (let [v (get-in-grid [x y])
        v (if (= v \S) \a v)]
    (->> dirs
         (map #(move [x y] %))
         (map (fn [p] [p (let [v# (get-in-grid p)]
                           (if (= v# \E) \z v#))]))
         (remove (fn [[_p v#]] (nil? v#)))
         (filter (fn [[_p v#]] (>= (inc (int v)) (int v#))))
         (map (fn [[p _]] [p 1]))
         (into {}))))

(defn solve-1
  []
  (let [grid (get-grid! input)
        start (find-start grid)
        end (find-end grid)]
    (-> (dijkstra start (partial neighbours (get-in-grid grid)))
        (get end))))

(defn solve-2
  []
  (let [grid (get-grid! input)
        getter (get-in-grid grid)
        end (find-end grid)]
    (->> (for [x (range (inc (count (get grid 0))))
               y (range (inc (count grid)))]
           [x y])
         (filter (fn [p] (contains? #{\S \a} (getter p))))
         (pmap (fn [p]
                 (-> (dijkstra p (partial neighbours getter))
                     (get end))))
         (remove nil?)
         (apply min))))
