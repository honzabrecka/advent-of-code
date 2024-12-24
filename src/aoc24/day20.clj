(ns aoc24.day20
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]))

(def input "./data/aoc24/day20.txt")

(defn parse-grid
  [grid]
  (let [grid (mapv vec grid)
        w (count (first grid))
        h (count grid)]
    [w h (into {} (for [y (range h)
                        x (range w)
                        :let [c (get-in grid [y x])]]
                    [[x y] c]))]))

(defn get-grid!
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (parse-grid))))

(defn find-start
  [grid]
  (first (for [[k v] grid :when (= v \S)] k)))

(defn find-end
  [grid]
  (first (for [[k v] grid :when (= v \E)] k)))

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
  [walls [x y]]
  (->> [[-1 0] [1 0] [0 1] [0 -1]]
       (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
       (filter (fn [p] (not (contains? walls p))))
       (map (fn [p] [p 1]))
       (into {})))

(defn group-walls
  [walls w h]
  (let [m (group-by (fn [[x y]]
                      (or (= x 0)
                          (= y 0)
                          (= x (dec w))
                          (= y (dec h))))
                    walls)]
    [(set (get m true))
     (set (get m false))]))

(defn solve-1
  []
  (let [[w h grid] (get-grid!)
        walls (->> grid
                   (filter (fn [[_ v]] (= v \#)))
                   (keys)
                   (set))
        start (find-start grid)
        end (find-end grid)
        _ (println start ">" end)
        m (dijkstra start #(neighbours walls %))
        score (get m end)
        [borders cheats] (group-walls walls w h)]
    (->> cheats
         (map (fn [w]
                (let [walls' (disj walls w)
                      m (dijkstra start #(neighbours walls' %))]
                  (- score (get m end)))))
         #_(frequencies)
         (sort >)
         (take-while #(>= % 100))
         (count))))
