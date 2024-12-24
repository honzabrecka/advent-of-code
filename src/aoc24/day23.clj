(ns aoc24.day23
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day23.txt")

(defn parse-line
  [line]
  (let [[a b] (clojure.string/split line #"-")]
    [[a b] [b a]]))

(defn get-input!
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (mapcat parse-line)
         (into []))))

(defn solve-1
  []
  (let [ps (get-input!)
        m (update-vals (group-by first ps) #(set (map second %)))
        ts (->> (keys m) (filter #(clojure.string/starts-with? % "t")) (set))]
    (->> ps
         (mapcat (fn [[a b]]
                   (let [common (clojure.set/intersection (get m a) (get m b))]
                     (->> common
                          (filter (fn [c]
                                    (let [cs (get m c)]
                                      (and (contains? cs a)
                                           (contains? cs b)))))
                          (map (fn [c] [a b c]))))))
         (map set)
         (set)
         (filter (fn [x]
                   (some #(contains? ts %) x)))
         (count))))

(defn solve-2
  []
  (let [ps (get-input!)
        m (update-vals (group-by first ps) #(set (map second %)))]
    (->> m
         (mapcat (fn [[a as]]
                   (->> (map #(conj (get m %) %) as)
                        (map #(clojure.set/intersection % (conj as a))))))
         (frequencies)
         (sort-by second >)
         (ffirst)
         (sort)
         (clojure.string/join ","))))
