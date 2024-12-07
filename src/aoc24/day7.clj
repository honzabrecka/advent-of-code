(ns aoc24.day7
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def input "./data/aoc24/day7.txt")

(defn parse-line
  [line]
  (let [[r ns] (clojure.string/split line #": ")]
    [(parse-long r)
     (->> (clojure.string/split ns #" ")
          (map parse-long))]))

(defn apply-ops
  [[n & ns] ops]
  (->> (map vector ns ops)
       (reduce (fn [acc [n op]] (op acc n)) n)))

(defn ||
  [a b]
  (parse-long (str a b)))

(defn can?
  [ops [r ns]]
  (->> (combo/selections ops (dec (count ns)))
       (some (fn [ops]
               (when (= r (apply-ops ns ops)) r)))))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (transduce (comp (map parse-line)
                     (map #(can? [+ *] %))
                     (remove nil?))
               + (line-seq reader))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (transduce (comp (map parse-line)
                     (map #(can? [+ * ||] %))
                     (remove nil?))
               + (line-seq reader))))
