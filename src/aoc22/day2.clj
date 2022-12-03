(ns aoc22.day2
  (:require [clojure.java.io :as io]))

(def input "./data/aoc22/day2.txt")

; 3 < 1 < 2 < 3
; 3, 1, 2, 3, 1, 2, 3, ...

(def rock 1)

(def paper 2)

(def scissors 3)

; aka wining move
(defn next-shape
  [x]
  (inc (mod x 3)))

; aka losing move
(defn prev-shape
  [x]
  (inc (mod (inc x) 3)))

(defn parse-line-1
  [s]
  (let [[_ a b] (re-matches #"^(A|B|C) (X|Y|Z)$" s)]
    (map {"A" rock
          "B" paper
          "C" scissors
          "X" rock
          "Y" paper
          "Z" scissors} [a b])))

(defn score-1
  [[a b]]
  (cond
      (= a b)              (+ 3 b)
      (= (next-shape a) b) (+ 6 b)
      :else                (+ 0 b)))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (transduce (comp (map parse-line-1)
                     (map score-1))
               +
               (line-seq reader))))

(defn parse-line-2
  [s]
  (let [[_ a b] (re-matches #"^(A|B|C) (X|Y|Z)$" s)]
    (map {"A" rock
          "B" paper
          "C" scissors
          "X" :lose
          "Y" :draw
          "Z" :win} [a b])))

(defn score-2
  [[a end]]
  (case end
    :draw (+ 3 a)
    :lose (+ 0 (prev-shape a))
    :win  (+ 6 (next-shape a))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (transduce (comp (map parse-line-2)
                     (map score-2))
               +
               (line-seq reader))))
