(ns aoc24.day15
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day15.test.txt")

(defn parse-grid
  [grid]
  (let [grid (mapv vec grid)
        w (count (first grid))
        h (count grid)]
    (into {} (for [y (range h)
                   x (range w)
                   :let [c (get-in grid [y x])]]
               [[x y] c]))))

(defn get-input!
  []
  (let [[grid moves] (with-open [reader (io/reader input)]
                       (->> (line-seq reader)
                            (partition-by #(clojure.string/starts-with? % "#"))
                            (doall)))]
    {:moves (seq (reduce str moves))
     :grid  (parse-grid grid)}))

(defn find-start
  [grid]
  (first (for [[k v] grid :when (= v \@)] k)))

(def move->delta
  {\^ [0 -1]
   \v [0 1]
   \> [1 0]
   \< [-1 0]})

(defn get-in-dir-1
  [grid [x y] [dx dy]]
  (loop [[x y] [x y]
         ps []]
    (let [x' (+ x dx)
          y' (+ y dy)
          v (get grid [x' y'])]
      (if (contains? #{\0 \[ \]} v)
        (recur [x' y'] (conj ps [x' y']))
        (conj ps [x' y'])))))

(defn move-boxes-1
  [grid [x y] [dx dy]]
  (let [ps (get-in-dir-1 grid [x y] [dx dy])]
    #_(println ps)
    (if (= (get grid (last ps)) \#)
      [grid [x y]]
      [(let [grid' (reduce (fn [grid p] (assoc grid p \O))
                           (assoc grid [x y] \.)
                           ps)]
         (assoc grid' (first ps) \@))
       (first ps)])))

(defn gps-1
  [grid]
  (reduce (fn [acc [[x y] v]]
            (if (= v \O)
              (+ acc (* 100 y) x)
              acc))
          0
          grid))

(defn solve-1
  []
  (let [{:keys [moves grid]} (get-input!)]
    (->> moves
         (reduce (fn [[grid [x y]] move]
                   (let [[dx dy] (move->delta move)
                         x' (+ x dx)
                         y' (+ y dy)]
                     (cond
                       ; do nothing
                       (= (get-in grid [y x]) \#)
                       [grid [x' y']]
                       ; move only cursor
                       (= (get-in grid [y x]) \.)
                       [(-> (assoc grid [x y] \.)
                            (assoc grid [x y] \@))
                        [x' y']]
                       ; move cursor + box(es)
                       :else
                       (move-boxes-1 grid [x y] [dx dy]))))
                 [grid (find-start grid)])
         first gps-1)))

(defn gps-2
  [grid]
  (reduce (fn [acc [[x y] v]]
            (if (= v \[)
              (+ acc (* 100 y) x)
              acc))
          0
          grid))

(defn multiply-grid
  [grid]
  (println grid)
  (map (fn [line]
         (mapcat (fn [c] (case c
                           \# [\# \#]
                           \. [\. \.]
                           \O [\[ \]]
                           \@ [\@ \.]))
                 (seq line)))
       grid))

(defn get-input-2!
  []
  (let [[grid moves] (with-open [reader (io/reader input)]
                       (->> (line-seq reader)
                            (partition-by #(clojure.string/starts-with? % "#"))
                            (doall)))]
    {:moves (seq (reduce str moves))
     :grid  (->> grid
                 #_(multiply-grid)
                 (parse-grid))}))

(defn move-boxes-2
  [grid [x y] [dx dy]]
  (let [ps (get-in-dir-1 grid [x y] [dx dy])]
    (println ps)
    (if (= (get grid (last ps)) \#)
      [grid [x y]]
      [(let [grid' (reduce (fn [grid p] (assoc grid p \O))
                           (assoc grid [x y] \.)
                           ps)]
         (assoc grid' (first ps) \@))
       (first ps)])))

(defn solve-2
  []
  (let [{:keys [moves grid]} (get-input-2!)]
    (->> moves
         (reduce (fn [[grid [x y]] move]
                   (let [[dx dy] (move->delta move)
                         x' (+ x dx)
                         y' (+ y dy)]
                     (cond
                       ; do nothing
                       (= (get-in grid [y x]) \#)
                       [grid [x' y']]
                       ; move only cursor
                       (= (get-in grid [y x]) \.)
                       [(-> (assoc grid [x y] \.)
                            (assoc grid [x y] \@))
                        [x' y']]
                       ; move cursor + box(es)
                       :else
                       (move-boxes-2 grid [x y] [dx dy]))))
                 [grid (find-start grid)])
         #_first
         #_gps-2)))
