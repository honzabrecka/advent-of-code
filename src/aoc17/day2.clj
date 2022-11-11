(ns aoc17.day2
  (:require [clojure.java.io :as io]))

(def input "./data/aoc17/day2.txt")

(defn get-input!
  [input]
  (with-open [reader (io/reader input)]
    (into []
          (map #(mapv parse-long (clojure.string/split % #"\s")))
          (line-seq reader))))

(defn solve-1
  []
  (->> (get-input! input)
       (map (fn [ns]
              (- (apply max ns)
                 (apply min ns))))
       (reduce +)))

(defn solve-2
  []
  (->> (get-input! input)
       (map (fn [ns]
              (first (for [x ns
                           y ns
                           :when (and (> x y) (zero? (mod x y)))
                           ]
                       (/ x y)))))
       (reduce +)))
