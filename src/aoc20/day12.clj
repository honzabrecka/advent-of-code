(ns aoc20.day12
  (:require [clojure.java.io :as io]))

(def input "./data/aoc20/day12.txt")

(defn parse
  [s]
  (let [[_ i n] (re-matches #"([NSEWLRF])(\d+)" s)]
    [i (Integer/parseInt n)]))

(def moves
  (letfn [(first [a b] a)]
    {"N" [first -]
     "S" [first +]
     "W" [- first]
     "E" [+ first]}))

(defn apply-move
  [direction n x y]
  (let [[map-x map-y] (get moves direction)]
    [(map-x x n) (map-y y n)]))

(defn rotate
  [d i n]
  (let [dirs ["N" "E" "S" "W"]
        c (->> (range)
               (drop-while #(not= d (get dirs %)))
               first)
        n# (/ n 90)
        f (get {"R" + "L" -} i)]
    (get dirs (mod (f c n#) (count dirs)))))

(defn manhattan-distance
  [^Integer x ^Integer y]
  (+ (Math/abs x) (Math/abs y)))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [[_ [x y]]
          (->> (line-seq reader)
               (map parse)
               (reduce (fn [[d [x y]] [i n]]
                         (cond
                           (contains? #{"N" "S" "W" "E"} i)
                           [d (apply-move i n x y)]
                           (= i "F")
                           [d (apply-move d n x y)]
                           (contains? #{"L" "R"} i)
                           [(rotate d i n) [x y]]))
                       ["E" [0 0]]))]
      (manhattan-distance x y))))

(defn rotate-point
  [[x y] angle]
  (let [s (Math/sin angle)
        c (Math/cos angle)]
    [(Math/round (- (* x c) (* y s)))
     (Math/round (+ (* x s) (* y c)))]))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [[[bx by]]
          (->> (line-seq reader)
               (map parse)
               (reduce (fn [[[bx by] [wx wy]] [i n]]
                         (cond
                           (contains? #{"N" "S" "W" "E"} i)
                           [[bx by] (apply-move i n wx wy)]
                           (= i "F")
                           [[(+ bx (* n wx)) (+ by (* n wy))] [wx wy]]
                           (= i "L")
                           [[bx by] (rotate-point [wx wy] (- (Math/toRadians n)))]
                           (= i "R")
                           [[bx by] (rotate-point [wx wy] (+ (Math/toRadians n)))]))
                       [[0 0] [10 -1]]))]
      (manhattan-distance bx by))))
