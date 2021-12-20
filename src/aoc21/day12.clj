(ns aoc21.day12
  (:require [clojure.java.io :as io]))

(def input "./data/aoc21/day12.txt")

(defn large?
  [s]
  (= s (clojure.string/upper-case s)))

(defn end?
  [s]
  (= s "end"))

(defn parse-line
  [s]
  (clojure.string/split s #"-"))

(defn get-input!
  [input]
  (let [pairs (with-open [reader (io/reader input)]
                (->> (line-seq reader)
                     (map parse-line)
                     (doall)))]
    (->> (concat pairs
                 (map (fn [[from to]] [to from]) pairs))
         (group-by first))))

(defn walk
  [graph path xs f]
  (->> xs
       (remove (fn [[_ to]] (and (not (large? to))
                                 (f path to))))
       (mapcat (fn [[_ to]]
                 (if (or (end? to)
                         (nil? to))
                   [(conj path to)]
                   (walk graph (conj path to) (get graph to) f))))))

(defn solve-1
  []
  (let [graph (get-input! input)]
    (walk graph ["start"] (get graph "start") (fn [path to]
                                                (contains? (set path) to)))))

(defn solve-2
  []
  (let [graph (get-input! input)]
    (walk graph ["start"] (get graph "start") (fn [path to]
                                                (or (= to "start")
                                                    (let [xs (->> (conj path to)
                                                                  (remove large?)
                                                                  frequencies
                                                                  vals)]
                                                      (or (> (count (filter #(> % 2) xs)) 0)
                                                          (> (count (filter #(> % 1) xs)) 1))))))))
