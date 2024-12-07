(ns aoc24.day5
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day5.txt")

(defn parse-rule
  [rule]
  (->> (clojure.string/split rule #"\|")
       (map parse-long)))

(defn parse-update
  [update]
  (->> (clojure.string/split update #",")
       (map parse-long)))

(defn get-input!
  []
  (with-open [reader (io/reader input)]
    (let [[rules updates] (->> (line-seq reader)
                               (filter #(not= % ""))
                               (partition-by #(clojure.string/includes? % "|"))
                               doall)]
      [(map parse-rule rules)
       (map parse-update updates)])))

(defn correct-order?
  [rules update]
  (let [m (zipmap update (range))]
    (every? (fn [[a b]]
              (let [a' (get m a)
                    b' (get m b)]
                (cond
                  (nil? a') true
                  (nil? b') true
                  :else (< a' b'))))
            rules)))

(defn order
  [rules]
  (->> rules
       (group-by first)
       (vals)
       (sort-by count >)
       (reduce (fn [acc [[a b] & xs]] (if (nil? xs) (conj acc a b) (conj acc a))) [])))

(defn fix-order
  [rules update]
  (let [update' (set update)]
    (->> rules
         (filter (fn [[a b]]
                   (and (contains? update' a)
                        (contains? update' b))))
         (order))))

#_[[97 13] [97 47] [75 29] [29 13] [97 29] [47 13] [75 47] [97 75] [47 29] [75 13]]

(defn middle
  [update]
  (nth update (int (/ (count update) 2))))

#_(let [[rules updates] (get-input!)
        update (nth updates 0)]
    (fix-order rules [61,29,13]))

(defn solve-1
  []
  (let [[rules updates] (get-input!)]
    (->> updates
         (filter #(correct-order? rules %))
         (map middle)
         (reduce +))))

(defn solve-2
  []
  (let [[rules updates] (get-input!)]
    (->> updates
         (remove #(correct-order? rules %))
         (map #(fix-order rules %))
         (map middle)
         (reduce +))))
