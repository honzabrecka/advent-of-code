(ns aoc20.day7
  (:require [clojure.java.io :as io]))

(def input "./data/aoc20/day7.txt")

(defn parse
  [record]
  (let [[[_ a b] & xs] (->> record
                            (re-seq #"((([0-9]) )?([a-z]+?) ([a-z]+?) bags?)")
                            (map #(drop 3 %)))
        m (into {}
                (comp (remove #(= [nil "no" "other"] %))
                      (map (fn [[n a b]]
                             [[a b] (Integer/parseInt n)])))
                xs)]
    [[a b] m]))

(defn create-graph!
  [input]
  (with-open [reader (io/reader input)]
    (into {} (map parse) (line-seq reader))))

(def shiny-gold ["shiny" "gold"])

(defn contains-any?
  [graph subject k]
  (if (= k subject)
    true
    (->> (keys (get graph k))
         (some #(contains-any? graph subject %)))))

(defn solve-1
  []
  (let [graph (create-graph! input)]
    (->> (keys graph)
         (filter #(not= shiny-gold %))
         (filter #(contains-any? graph shiny-gold %))
         (count))))

(defn count-inner-bags
  [graph node]
  (map (fn [[k v]]
         (* v (->> (get graph k)
                   (count-inner-bags graph)
                   (reduce + 1))))
       node))

(defn solve-2
  []
  (let [graph (create-graph! input)]
    (->> (get graph shiny-gold)
         (count-inner-bags graph)
         (reduce +))))
