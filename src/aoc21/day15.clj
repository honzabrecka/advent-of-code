(ns aoc21.day15
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]))

(def input "./data/aoc21/day15.txt")

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.

  Returns a map from nodes to their distance from start."
  [start _ f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (update-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn get-map!
  [input]
  (with-open [reader (io/reader input)]
    (into []
          (map #(mapv parse-long (clojure.string/split % #"")))
          (line-seq reader))))

(defn neighbours
  [m [x y]]
  (into {}
        (comp (map (fn [[x y]]
                     [[x y] (get-in m [y x])]))
              (remove #(nil? (second %))))
        (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]]
          [(+ x dx) (+ y dy)])))

(defn solve-1
  []
  (let [m (get-map! input)
        start [0 0]
        end [(dec (count (last m))) (dec (count m))]]
    (get (dijkstra start end (partial neighbours m)) end)))

(defn inc#
  [n]
  (let [n' (inc n)]
    (if (= n' 10) 1 n')))

(defn enlarge
  [m n]
  (let [row (mapv (fn [row] (vec (flatten (take n (iterate #(mapv inc# %) row))))) m)]
    (vec (mapcat identity (take n (iterate (fn [cols]
                                             (mapv #(mapv inc# %) cols)) row))))))

(defn solve-2
  []
  (let [m (enlarge (get-map! input) 5)
        start [0 0]
        end [(dec (count (last m))) (dec (count m))]]
    (get (dijkstra start end (partial neighbours m)) end)))
