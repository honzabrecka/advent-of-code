(ns aoc22.day1
  (:require [clojure.java.io :as io]
            [aoc22.shared :as shared]))

(def input "./data/aoc22/day1.txt")

#_(def partition-by-nil
  (comp (partition-by nil?)
        (remove #(->> % first nil?))))

(defn get-calories!
  [input]
  (with-open [reader (io/reader input)]
    (into [] (shared/section-seq parse-long +) (line-seq reader))))

(defn solve-1
  []
  (->> (get-calories! input)
       (sort >)
       first))

(defn solve-2
  []
  (->> (get-calories! input)
       (sort >)
       (take 3)
       (reduce +)))
