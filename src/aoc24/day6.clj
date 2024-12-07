(ns aoc24.day6
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day6.txt")

(defn get-grid!
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map vec)
         (into []))))

(defn compact-grid
  [grid]
  (let [w (count (first grid))
        h (count grid)]
    (->> (for [x (range w)
               y (range h)]
           [x y])
         (reduce (fn [acc [x y]]
                   (let [v (get-in grid [y x])]
                     (case v
                       \# (update acc :obstacles #(conj % [x y]))
                       \. (update acc :empty #(conj % [x y]))
                       (assoc acc :guard [[x y] v]))))
                 {:guard nil :obstacles #{} :empty #{} :w w :h h}))))

(defn move-guard
  [[[x y] v]]
  (case v
    \^ [[x (dec y)] v]
    \> [[(inc x) y] v]
    \< [[(dec x) y] v]
    \v [[x (inc y)] v]))

(defn shift
  [[x & xs]]
  (concat xs [x]))

(defn rotate-guard
  [v]
  (let [dirs [\^ \> \v \<]]
    (get (zipmap dirs (shift dirs)) v)))

(defn out?
  [w h [x y]]
  (or (< x 0)
      (< y 0)
      (> x w)
      (> y h)))

(defn move
  [{:keys [guard obstacles w h]}]
  (loop [guard' guard
         seen #{(first guard)}
         hit #{}]
    (let [[p v] (move-guard guard')]
      (cond
        (contains? hit [p v]) [seen :loop]
        (contains? obstacles p) (recur [(first guard') (rotate-guard v)] seen (conj hit [p v]))
        (out? w h p) [seen :out]
        :else (recur [p v] (conj seen p) hit)))))

(defn solve-1
  []
  (->> (get-grid!)
       (compact-grid)
       (move)
       (first)
       (count)
       ; dec because it includes step outside the grid
       (dec)))

(defn solve-2
  []
  (let [grid (compact-grid (get-grid!))
        [seen] (move grid)]
    (->> (clojure.set/intersection (:empty grid) seen)
         (map (fn [added]
                (->> (update grid :obstacles #(conj % added))
                     (move))))
         (filter (fn [[_ type]] (= type :loop)))
         (count))))
