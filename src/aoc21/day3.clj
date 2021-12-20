(ns aoc21.day3
  (:require [clojure.java.io :as io]))

(def input "./data/aoc21/day3.txt")

(def to-bit
  {\0 0
   \1 1})

(defn parse-line
  [s]
  (into [] (map to-bit) s))

(defn get-matrix!
  [input]
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         doall)))

(defn rotate-matrix
  [matrix]
  (apply map vector matrix))

(defn to-dec
  [col]
  (->> (reverse col)
       (reduce (fn [[acc i] b]
                 [(if (= b 1) (bit-set acc i) acc) (inc i)])
               [0 0])
       first))

(defn more
  [m]
  (let [z (get m 0 0)
        o (get m 1 0)]
    (if (> o z) 1 0)))

(defn less
  [m]
  (let [z (get m 0 0)
        o (get m 1 0)]
    (if (> z o) 1 0)))

(defn sort-bits
  [selector matrix]
  (->> matrix
       (rotate-matrix)
       (map frequencies)
       (map selector)))

(defn solve-1
  []
  (let [matrix (get-matrix! input)]
    (* (to-dec (sort-bits more matrix))
       (to-dec (sort-bits less matrix)))))

(defn more2
  [m]
  (let [z (get m 0 0)
        o (get m 1 0)]
    (if (= z o)
      1
      (if (> o z) 1 0))))

(defn less2
  [m]
  (let [z (get m 0 0)
        o (get m 1 0)]
    (if (= z o)
      0
      (if (> z o) 1 0))))

(defn match-by-bits
  [selector matrix]
  (loop [cols matrix
         i 0]
    (let [bits (sort-bits selector cols)
          cols# (filter (fn [col]
                          (= (nth col i) (nth bits i)))
                        cols)]
      (if (= (count cols#) 1)
        (first cols#)
        (recur cols# (inc i))))))

(defn solve-2
  []
  (let [matrix (get-matrix! input)]
    (* (to-dec (match-by-bits more2 matrix))
       (to-dec (match-by-bits less2 matrix)))))
