(ns aoc24.day13
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day13.txt")

(defn get-coords
  [s]
  (let [[_ x y] (re-matches #".+?: X[\+\=]([\d]+), Y[\+\=]([\d]+)" s)]
    [(parse-long x) (parse-long y)]))

(defn parse-line
  [[a b prize]]
  [(get-coords a)
   (get-coords b)
   (get-coords prize)])

(defn get-input!
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (partition-by #{""})
         (remove #(= % [""]))
         (map parse-line)
         (into []))))

(defn =push
  [getter a# b# a b prize]
  (and (= (+ (* a# (getter a))
             (* b# (getter b)))
          (getter prize))))

(defn get-pushes
  [[a b prize]]
  (first
    (for [a# (range 100)
          b# (range 100)
          :when (and (=push first a# b# a b prize)
                     (=push second a# b# a b prize))]
      [a# b#])))

(defn solve-1
  []
  (->> (get-input!)
       (map get-pushes)
       (remove empty?)
       (map (fn [[a b]] (+ (* a 3) b)))
       (reduce +)))

;;;

(def n 10000000000000)

(defn f
  [[button-a button-b prize]]
  (let [eq1 [(first button-a) (first button-b) (first prize)]
        eq2 [(second button-a) (second button-b) (second prize)]
        eq1 (->> eq1
                (map #(* % (second button-a)))
                (map #(* % -1))
                (drop 1))
        eq2 (->> eq2
                (map #(* % (first button-a)))
                (drop 1))
        [l r] (map + eq1 eq2)
        b (/ r l)
        a (/ (- (second prize) (* b (second button-b))) (second button-a))]
    [a b]))

(defn long?
  [n]
  (= n (long n)))

(defn solve-1
  []
  (->> (get-input!)
       (map f)
       (filter (fn [[a b]] (and (long? a) (long? b))))
       (map (fn [[a b]] (+ (* a 3) b)))
       (reduce +)))

(defn solve-2
  []
  (->> (get-input!)
       (map (fn [[a b prize]] [a b (map #(+ % n) prize)]))
       (map f)
       (filter (fn [[a b]] (and (long? a) (long? b))))
       (map (fn [[a b]] (+ (* a 3) b)))
       (reduce +)))


; 94a+22b=8400
; 34a+67b=5400

; 47a+11b=4200
; 34a+67b=5400

; 1598a+374b=142800
; 1598a+3149b=253800

; 3523b=396600
; b=
