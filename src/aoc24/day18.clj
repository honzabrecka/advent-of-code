(ns aoc24.day18
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]))

(def input "./data/aoc24/day18.txt")

(defn parse-line
  [line]
  (->> (clojure.string/split line #",")
       (mapv parse-long)))

(defn get-bytes!
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (into []))))

(defn go
  ([bytes w h start end]
   (go bytes w h start end #{start}))
  ([bytes w h [x y] end seen]
   (if (= [x y] end)
     [(count seen)]
     (->> [[-1 0] [0 -1] [1 0] [0 1]]
          (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
          (filter (fn [[x y]]
                    (and (>= x 0)
                         (<= x w)
                         (>= y 0)
                         (<= y h)
                         (not (contains? seen [x y]))
                         (not (contains? bytes [x y])))))
          (mapcat (fn [p] (go bytes w h p end (conj seen p))))))))

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
  [bytes w h [x y]]
  (->> [[-1 0] [0 -1] [1 0] [0 1]]
       (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
       (filter (fn [[x y]]
                 (and (>= x 0)
                      (<= x w)
                      (>= y 0)
                      (<= y h)
                      (not (contains? bytes [x y])))))
       (map (fn [p] [p 1]))
       (into {})))

(defn solve-1
  []
  (let [w 70
        h 70
        bytes (->> (get-bytes!) (take 1024) (set))
        start [0 0]
        end [w h]]
    (-> (dijkstra start (fn [[x y]] (neighbours bytes w h [x y])))
        (get end))
    #_(->> (go bytes w h start end)
         (sort <)
         (first)
         (dec))))

(defn solve-2
  []
  (let [w 70
        h 70
        bytes (get-bytes!)
        start [0 0]
        end [w h]]
    (->> (range (count bytes))
         (some (fn [n]
                 (let [bytes' (set (take n bytes))
                       m (dijkstra start
                                   (fn [[x y]] (neighbours bytes' w h [x y])))]
                   (when (nil? (get m end))
                     (get bytes (dec n)))))))))
