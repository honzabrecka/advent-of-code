(ns aoc20.day11
  (:require [clojure.java.io :as io]))

(def input "./data/aoc20/day11.txt")

(defn parse
  [s]
  (into [] s))

(def directions
  (memoize
    (fn []
      (let [r [-1 0 1]]
        (for [x r y r :when (not= [x y] [0 0])]
          [x y])))))

(defn floor?
  [x]
  (= x \.))

(defn empty-seat?
  [x]
  (= x \L))

(defn occupied-seat?
  [x]
  (= x \#))

(defn adjacent-seat-positions
  [grid x y]
  (->> (directions)
       (map (fn [[plus-x plus-y]]
              [(+ plus-x x) (+ plus-y y)]))
       (remove (fn [[x y]]
                 (nil? (get-in grid [y x]))))))

(defn first-visible-seat-position
  [grid initial-pos [plus-x plus-y]]
  (->> (range)
       (reductions (fn [[x y] _] [(+ x plus-x) (+ y plus-y)]) initial-pos)
       (drop 1)
       (drop-while (fn [[x y]] (floor? (get-in grid [y x]))))
       (take 1)
       first))

(defn visible-seat-positions
  [grid x y]
  (->> (directions)
       (map #(first-visible-seat-position grid [x y] %))
       (remove (fn [[x y]]
                 (nil? (get-in grid [y x]))))))

(defn find-first-unchanged-shift
  [shift initial-grid]
  (->> (range)
       (reductions (fn [grid _] (shift grid)) initial-grid)
       (partition 2 1)
       (drop-while (fn [[prev current]] (not= prev current)))
       (take 1)
       (ffirst)))

(defn non-floor-seat-positions
  [grid adjacent-seats-selector]
  (let [width (count (first grid))
        height (count grid)
        positions (for [x (range width)
                        y (range height)]
                    [x y])]
    (->> positions
         (map (fn [[x y]]
                (let [seat (get-in grid [y x])]
                  [[x y]
                   (if (floor? seat) [] (adjacent-seats-selector grid x y))])))
         (remove (fn [[_ adjacent-seats]] (= adjacent-seats []))))))

(defn shift
  [positions change]
  (fn [initial-grid]
    (reduce (fn [grid [[x y] adjacent-seats]]
              (let [seat (get-in initial-grid [y x])
                    seats (map (fn [[x y]] (get-in initial-grid [y x])) adjacent-seats)
                    seat# (change seat seats)]
                (assoc-in grid [y x] seat#)))
            initial-grid
            positions)))

(defn change-1
  [seat seats]
  (cond
    (and (empty-seat? seat) (not-any? occupied-seat? seats))
    \#
    (and (occupied-seat? seat) (>= (count (filter occupied-seat? seats)) 4))
    \L
    :else
    seat))

(defn change-2
  [seat seats]
  (cond
    (and (empty-seat? seat) (not-any? occupied-seat? seats))
    \#
    (and (occupied-seat? seat) (>= (count (filter occupied-seat? seats)) 5))
    \L
    :else
    seat))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [grid (into [] (map parse) (line-seq reader))
          positions (non-floor-seat-positions grid adjacent-seat-positions)]
      (->> (find-first-unchanged-shift (shift positions change-1) grid)
           (flatten)
           (filter occupied-seat?)
           (count)))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [grid (into [] (map parse) (line-seq reader))
          positions (non-floor-seat-positions grid visible-seat-positions)]
      (->> (find-first-unchanged-shift (shift positions change-2) grid)
           (flatten)
           (filter occupied-seat?)
           (count)))))
