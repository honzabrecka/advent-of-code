(ns aoc21.day9
  (:require [clojure.java.io :as io]))

(def input "./data/aoc21/day9.txt")

(defn parse-line
  [s]
  (mapv parse-long (clojure.string/split s #"")))

(defn get-input!
  [input]
  (with-open [reader (io/reader input)]
    (into []
          (map parse-line)
          (line-seq reader))))

(def d [[0 -1]
        [0 1]
        [-1 0]
        [1 0]])

(defn adjacents
  [matrix]
  (let [w (count (get matrix 0))
        h (count matrix)]
    (for [x (range w)
          y (range h)]
      [(get-in matrix [y x])
       (into #{}
             (comp (map (fn [[dx dy]]
                          (get-in matrix [(+ y dy) (+ x dx)])))
                   (remove nil?))
             d)
       [x y]])))

(defn solve-1
  []
  (->> (get-input! input)
       (adjacents)
       (filter (fn [[v ads _]]
                 (every? #(< v %) ads)))
       (map (fn [[v _ _]] (inc v)))
       (reduce +)))

(defn basin
  ([matrix point]
   (basin matrix point (atom #{point})))
  ([matrix [x y] result]
   (doseq [[_ p] (->> d
                      (map (fn [[dx dy]]
                             (let [x# (+ x dx)
                                   y# (+ y dy)]
                               [(get-in matrix [y# x#]) [x# y#]])))
                      (remove (fn [[v p]]
                                (or (nil? v)
                                    (= v 9)
                                    (contains? @result p)))))]
     (swap! result conj p)
     (basin matrix p result))
   @result))

(defn solve-2
  []
  (let [matrix (get-input! input)]
    (->> matrix
         (adjacents)
         (filter (fn [[v ads _]]
                   (every? #(< v %) ads)))
         (map (fn [[_ _ p]]
                (basin matrix p)))
         (map count)
         (sort >)
         (take 3)
         (reduce *))))
