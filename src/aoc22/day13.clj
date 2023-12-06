(ns aoc22.day13
  (:require [clojure.java.io :as io]
            [aoc22.shared :as shared]))

(def input "./data/aoc22/day13.txt")

(defn right-order?
  [[a & as] [b & bs]]
  (cond
    (and (nil? a) (nil? b)) :continue
    (nil? a) true
    (nil? b) false
    (and (int? a) (int? b) (< a b)) true
    (and (int? a) (int? b) (> a b)) false
    (and (int? a) (int? b)) (recur as bs)
    (and (coll? a) (coll? b))
    (let [result (right-order? a b)]
      (cond
        (true? result) true
        (false? result) false
        :else (recur as bs)))
    (coll? a)
    (let [result (right-order? a [b])]
      (cond
        (true? result) true
        (false? result) false
        :else (recur as bs)))
    (coll? b)
    (let [result (right-order? [a] b)]
      (cond
        (true? result) true
        (false? result) false
        :else (recur as bs)))))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [pairs (into []
                      (shared/section-seq read-string)
                      (line-seq reader))]
      (->> pairs
           (keep-indexed vector)
           (filter (fn [[_ pair]] (apply right-order? pair)))
           (map first)
           (map inc)
           (reduce +)))))

(defn decode-key
  [[[a _] [b _]]]
  (* (inc a) (inc b)))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [packets #{[[2]] [[6]]}]
      (->> (line-seq reader)
           (remove #(= % ""))
           (map read-string)
           (into packets)
           (sort right-order?)
           (keep-indexed vector)
           (filter (fn [[_ v]] (contains? packets v)))
           decode-key))))
