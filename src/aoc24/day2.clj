(ns aoc24.day2
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day2.txt")

(defn parse-line
  [line]
  (->> (clojure.string/split line #"[ ]+")
       (map parse-long)))

(defn safe-diff?
  [d]
  (and (> d 0) (< d 4)))

(defn safe-inc?
  [a b]
  (safe-diff? (- b a)))

(defn safe-dec?
  [a b]
  (safe-diff? (- a b)))

(defn safe?
  [pred col]
  (loop [[x & xs] col
         prev-x nil]
    (cond
      (nil? x) true
      (nil? prev-x) (recur xs x)
      (pred prev-x x) (recur xs x)
      :else false)))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (filter #(or (safe? safe-inc? %) (safe? safe-dec? %)))
         count)))

(defn safe-with-toleration?
  [pred col]
  (loop [[x & xs] col
         prev-x nil
         prev []]
    (cond
      (nil? x) true
      (nil? prev-x) (recur xs x prev)
      (pred prev-x x) (recur xs x (conj prev prev-x))
      :else (or (safe? pred (concat prev [prev-x] xs))
                (safe? pred (concat prev [x] xs))))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (filter #(or (safe-with-toleration? safe-inc? %)
                      (safe-with-toleration? safe-dec? %)))
         count)))
