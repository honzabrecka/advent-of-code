(ns aoc23.day11
  (:require [clojure.java.io :as io]))

(def input "./data/aoc23/day11.txt")

(defn get-in-grid
  [grid [x y]]
  (get-in grid [y x]))

(defn galaxies
  [grid]
  (let [w (count (get grid 0))
        h (count grid)]
    (for [x (range w)
          y (range h)
          :when (= (get-in-grid grid [x y]) \#)]
      [x y])))

#_(defn empty
  [grid galaxies]
  (let [w (count (get grid 0))
        h (count grid)
        xs (into #{} (map first galaxies))
        ys (into #{} (map second galaxies))]
    [(clojure.set/difference (into #{} (range w)) xs)
     (clojure.set/difference (into #{} (range h)) ys)]))

(defn empty
  [grid galaxies]
  (let [w (count (get grid 0))
        h (count grid)
        xs (into #{} (map first galaxies))
        ys (into #{} (map second galaxies))]
    [(into #{} (remove #(contains? xs %)) (range w))
     (into #{} (remove #(contains? ys %)) (range h))]))

(defn get-grid!
  []
  (with-open [reader (io/reader input)]
    (let [grid (->> (line-seq reader) (map vec) (into []))
          galaxies (galaxies grid)
          empty (empty grid galaxies)]
      {:grid     grid
       :galaxies galaxies
       :empty    empty})))

(defn galaxies-pairs
  [galaxies]
  (into #{}
        (for [a galaxies
              b galaxies
              :when (not= a b)]
          #{a b})))

(defn shift
  [age [xs ys] [x y]]
  (let [d (fn [vs v]
            (count (filter #(< % v) vs)))]
    [(+ x (* (dec age) (d xs x)))
     (+ y (* (dec age) (d ys y)))]))

(defn solve-1
  []
  (let [grid (get-grid!)
        pairs (galaxies-pairs (grid :galaxies))]
    (->> pairs
         (map vec)
         (map (fn [[a b]]
                [(shift 2 (grid :empty) a)
                 (shift 2 (grid :empty) b)]))
         (map (fn [[[ax ay] [bx by]]]
                [(abs (- ax bx))
                 (abs (- ay by))]))
         (map (fn [[dx dy]] (+ dx dy)))
         (reduce +))))

(defn solve-2
  []
  (let [grid (get-grid!)
        pairs (galaxies-pairs (grid :galaxies))]
    #_(transduce (comp (map vec)
                     (map (fn [[a b]]
                            [(shift 1000000 (grid :empty) a)
                             (shift 1000000 (grid :empty) b)]))
                     (map (fn [[[ax ay] [bx by]]]
                            [(abs (- ax bx))
                             (abs (- ay by))]))
                     (map (fn [[dx dy]] (+ dx dy))))
               + pairs)
    (->> pairs
         (map vec)
         (map (fn [[a b]]
                [(shift 1000000 (grid :empty) a)
                 (shift 1000000 (grid :empty) b)]))
         (map (fn [[[ax ay] [bx by]]]
                [(abs (- ax bx))
                 (abs (- ay by))]))
         (map (fn [[dx dy]] (+ dx dy)))
         (reduce +))))
