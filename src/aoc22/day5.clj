(ns aoc22.day5
  (:require [clojure.java.io :as io]
            [aoc22.shared :as shared]))

(def input "./data/aoc22/day5.txt")

(defn parse-move
  [s]
  (->> (re-matches #"^move ([\d]+) from ([\d]+) to ([\d]+)$" s)
       (drop 1)
       (map parse-long)))

(defn space?
  [s]
  (= s \space))

(defn stack
  [stacks i]
  (reduce (fn [acc stack]
            (let [x (get stack i)]
              (if (or (space? x) (nil? x))
                (reduced acc)
                (conj acc x))))
          [] stacks))

(defn get-input!
  [input]
  (with-open [reader (io/reader input)]
    (let [[stacks moves] (into [] (shared/section-seq identity) (line-seq reader))
          [indices & reversed-stacks] (reverse stacks)
          stacks-indices (->> indices
                              (keep-indexed (fn [i s] (if (space? s) nil i)))
                              (remove nil?))]
      [(reduce (fn [acc i]
                 (conj acc (stack reversed-stacks i)))
               [] stacks-indices)
       (map parse-move moves)])))

(defn take-from-top
  [n coll]
  (split-at (- (count coll) n) coll))

(defn solve-1
  []
  (let [[stacks moves] (get-input! input)]
    (->> (reduce (fn [stacks [count from to]]
                   (let [[stays moves] (take-from-top count (get stacks (dec from)))]
                     (-> stacks
                         (assoc (dec from) stays)
                         (update (dec to) #(concat % (reverse moves))))))
                 stacks moves)
         (map last)
         (reduce str))))

(defn solve-2
  []
  (let [[stacks moves] (get-input! input)]
    (->> (reduce (fn [stacks [count from to]]
                   (let [[stays moves] (take-from-top count (get stacks (dec from)))]
                     (-> stacks
                         (assoc (dec from) stays)
                         (update (dec to) #(concat % moves)))))
                 stacks moves)
         (map last)
         (reduce str))))
