(ns aoc20.day14
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def input "./data/aoc20/day14.txt")

(defn parse
  [line]
  (if (clojure.string/starts-with? line "mask")
    (let [[_ mask] (re-matches #"mask = ([X01]+)" line)]
      [:mask (into [] mask)])
    (let [[_ index value] (re-matches #"mem\[([\d]+)\] = ([\d]+)" line)]
      [:mem (Long/parseLong index) (Long/parseLong value)])))

(defn to-binary
  [n]
  (Integer/toBinaryString n))

(defn to-decimal
  [n]
  (Long/parseLong (reduce str n) 2))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse)
         (reduce (fn [[memory mask] [x a b]]
                   (if (= x :mask)
                     [memory a]
                     (let [binary (into [] (to-binary b))
                           row (concat (repeat (- (count mask)
                                                  (count binary)) \0)
                                       binary)
                           b# (->> (map vector mask row)
                                   (map (fn [[m b]]
                                          (if (= m \X) b m))))]
                       [(assoc memory a (to-decimal b#)) mask])))
                 [{} nil])
         first
         vals
         (reduce +))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse)
         #_(take 2)
         (reduce (fn [[memory mask] [x a b]]
                   (if (= x :mask)
                     [memory a]
                     (let [binary (into [] (to-binary a))
                           row (concat (repeat (- (count mask)
                                                  (count binary)) \0)
                                       binary)
                           a# (->> (map vector mask row)
                                   (mapv (fn [[m b]]
                                           (case m
                                             \0 b
                                             \1 m
                                             \X m))))
                           xs (into [] (comp (keep-indexed (fn [i x] [i x]))
                                             (filter (fn [[_ x]] (= x \X)))
                                             (map first))
                                    a#)
                           cxs (count xs)
                           pcs (combo/permuted-combinations (concat (repeat cxs \0) (repeat cxs \1)) cxs)]


                       [(reduce (fn [memory# pcs#]
                                  (let [zip (map vector xs pcs#)
                                        i (reduce (fn [acc [i v]]
                                                    (assoc acc i v))
                                                  a# zip)]
                                    (assoc memory# (to-decimal i) b)))
                                memory pcs)
                        mask])))
                 [{} nil])
         first
         vals
         (reduce +))))
