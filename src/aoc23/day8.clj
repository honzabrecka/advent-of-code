(ns aoc23.day8
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]))

(def input "./data/aoc23/day8.txt")

(defn get-input!
  []
  (with-open [reader (io/reader input)]
    (doall (line-seq reader))))

(defn parse-line
  [line]
  (let [[key vals] (clojure.string/split line #" = ")]
    [key (clojure.string/split (subs vals 1 (dec (count vals))) #", ")]))

(defn get-parsed-input!
  []
  (let [[dirs _ & ms] (get-input!)]
    {:dirs (map #(if (= \L %) first second) dirs)
     :m    (into {} (map parse-line ms))}))

(defn count-between
  [{:keys [dirs m]} start end?]
  (let [c (count dirs)]
    (loop [k start
           i 0]
      (let [d (nth dirs (mod i c))
            v (d (get m k))]
        (if (end? v)
          (inc i)
          (recur v (inc i)))))))

(defn solve-1
  []
  (count-between (get-parsed-input!) "AAA" #(= % "ZZZ")))

(defn solve-2
  []
  (let [input (get-parsed-input!)]
    (->> (input :m)
         (keys)
         (filter #(clojure.string/ends-with? % "A"))
         (pmap (fn [start]
                 (count-between input start #(clojure.string/ends-with? % "Z"))))
         (reduce math/lcm))))
