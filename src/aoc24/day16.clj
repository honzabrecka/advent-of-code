(ns aoc24.day16
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]))

(def input "./data/aoc24/day16.txt")

(defn parse-grid
  [grid]
  (let [grid (mapv vec grid)
        w (count (first grid))
        h (count grid)]
    (into {} (for [y (range h)
                   x (range w)
                   :let [c (get-in grid [y x])]]
               [[x y] c]))))

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

(def neighbours
  (memoize
    (fn neighbours
      [grid [x y] o]
      (->> [[[-1 0] \<] [[1 0] \>] [[0 1] \v] [[0 -1] \^]]
           (map (fn [[[dx dy] o]] [[(+ x dx) (+ y dy)] o]))
           (filter (fn [[[x y] o]]
                     (not (contains? grid [x y]))))
           (map (fn [[p o']] [[p o'] (if (= o o') 1 1001)]))
           (into {})))))


(defn solve-1
  []
  (let [grid (get-grid!)
        grid' (->> grid
                   (filter (fn [[_ v]] (= v \#)))
                   (keys)
                   (set))
        start (find-start grid)
        end (find-end grid)
        m (dijkstra [start \>]
                    (fn [[[x y] o]]
                      (neighbours grid' [x y] o)))]
    (->> m
         (filter (fn [[[p o] v]] (= p end)))
         (map second)
         (sort)
         (first))))

(defn go
  ([grid g [start o] end]
   (go grid g [start o] end #{start}))
  ([grid g [[x y] o] end seen]
   (let [ns (->> [[[-1 0] \<] [[1 0] \>] [[0 1] \v] [[0 -1] \^]]
                 (map (fn [[[dx dy] o]] [[(+ x dx) (+ y dy)] o]))
                 (filter (fn [[p o]] (and (not (contains? (set seen) p))
                                          (not (contains? grid p)))))
                 (filter g))]
     (if (empty? ns)
       [seen]
       (mapcat (fn [[p o]] (go grid g [p o] end (conj seen p))) ns)))))

(defn solve-2
  []
  (let [grid (get-grid!)
        grid' (->> grid
                   (filter (fn [[_ v]] (= v \#)))
                   (keys)
                   (set))
        start (find-start grid)
        _ (println "start:" start)
        end (find-end grid)
        _ (println "end:" end)
        m (dijkstra [start \>]
                    (fn [[[x y] o]]
                      (neighbours grid' [x y] o)))
        [end' score] (->> m
                          (filter (fn [[[p o] v]] (= p end)))
                          (sort-by second)
                          (first))
        _ (println "end + score:" end' score)]
    (->> m
         (pmap (fn [[p s]]
                 (let [m (dijkstra p (fn [[[x y] o]]
                                       (neighbours grid' [x y] o)))
                       end-score (get m end')]
                   [p (= (+ end-score s) score)])))
         (filter (fn [[_ r]] (true? r)))
         (map ffirst)
         (set)
         (count))))

(defn solve-2
  []
  (let [grid (get-grid!)
        grid' (->> grid
                   (filter (fn [[_ v]] (= v \#)))
                   (keys)
                   (set))
        start (find-start grid)
        _ (println "start:" start)
        end (find-end grid)
        _ (println "end:" end)
        m (dijkstra [start \>]
                    (fn [[[x y] o]]
                      (neighbours grid' [x y] o)))
        [end' score] (->> m
                          (filter (fn [[[p o] v]] (= p end)))
                          (sort-by second)
                          (first))
        _ (println "end + score:" end' score)]
    (->> (go grid'
             (memoize
               (fn [p]
                 (let [m' (dijkstra p (fn [[[x y] o]]
                                        (neighbours grid' [x y] o)))
                       end-score (get m' end')]
                   (= (+ end-score (get m p)) score))))
             [start \>]
             end)
         (reduce clojure.set/union)
         (count))))

