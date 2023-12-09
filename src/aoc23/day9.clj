(ns aoc23.day9
  (:require [clojure.java.io :as io]))

(def input "./data/aoc23/day9.txt")

(defn parse-line
  [line]
  (map parse-long (clojure.string/split line #" ")))

(defn extrapolate
  [target xs]
  (let [t {:next {:diff +
                  :pick last}
           :prev {:diff (fn [a b] (- b a))
                  :pick first}}
        {:keys [diff pick]} (get t target)]
    (loop [xs xs
           result [(pick xs)]]
      (let [xs' (->> (partition 2 1 xs)
                     (map (fn [[a b]] (- b a))))
            result' (conj result (pick xs'))]
        (if (every? zero? xs')
          (reduce diff (reverse result'))
          (recur xs' result'))))))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (map #(extrapolate :next %))
         (reduce +))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (map #(extrapolate :prev %))
         (reduce +))))
