(ns aoc19.day10
  (:require [clojure.java.io :as io]))

(defn input
  []
  (with-open [reader (io/reader "./data/aoc19/day10.txt")]
    (into [] (map #(clojure.string/split % #"")) (line-seq reader))))

(defn points
  [input]
  (->> (for [x (range (count (nth input 0)))
             y (range (count input))]
         [x y])
       (map (fn [[x y]]
              (let [v (get-in input [y x])]
                (if (= v ".") nil [x y]))))
       (filter (complement nil?))))

(defn q
  [x y]
  (if (> x 0)
    (if (> y 0) 3 2)
    (if (> y 0) 4 1)))

(defn div
  [a b]
  (if (zero? b)
    (if (>= a 0) :inf+ :inf-)
    (/ a b)))

(defn can-see-count
  [points [cx cy]]
  (->> points
       (map (fn [[x y]] [(- cx x) (- cy y)]))
       (filter (fn [[x y]] (or (not= x 0) (not= y 0))))
       (map (fn [[x y]] [(q x y) (div x y)]))
       (set)
       (count)))

(defn main1a
  []
  (let [points# (points (input))]
    (->> points#
         (map (fn [point]
                [point (can-see-count points# point)]))
         (sort-by (fn [[_ count]] count))
         (last))))

(defn manhattan-distance
  [x y]
  (+ (Math/abs x)
     (Math/abs y)))

(defn n360
  [d]
  (mod (+ (mod d 360) 360) 360))

(defn f
  [points cx cy]
  (->> points
       (filter (fn [point] (not= point [cx cy])))
       (map (fn [[x y]]
              [x y (n360 (* (Math/atan2 (- x cx)
                                        (- (- y cy)))
                            (/ 180 Math/PI)))]))
       (group-by (fn [[_ _ rotation]] rotation))
       (map (fn [[rotation vs]]
              [rotation (sort-by (fn [[x y _]]
                                   (manhattan-distance (- cy y)
                                                       (- cx x)))
                                 vs)]))
       (sort-by (fn [[rotation _]] rotation))))

(defn main1b
  []
  (let [points# (points (input))]
    (->> points#
         (map (fn [[x y]]
                [[x y] (count (f points# x y))]))
         (sort-by (fn [[_ count]] count))
         (last))))

(defn main2
  []
  (-> (input) (points) (f 19 11) (nth 199)))

(defn -main
  []
  (println (main1a))
  (println (main1b))
  (println (main2)))
