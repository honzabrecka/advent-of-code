(ns aoc21.day5
  (:require [clojure.java.io :as io]))

(def input "./data/aoc21/day5.txt")

(defn parse-line
  [s]
  (let [[a b] (clojure.string/split s #" -> ")
        parse-pair (fn [s] (mapv parse-long (clojure.string/split s #",")))]
    [(parse-pair a) (parse-pair b)]))

(defn get-input!
  [input]
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (doall))))

(defn sorted-range
  [a b]
  (if (> b a)
    (range a (inc b))
    (reverse (range b (inc a)))))

(defn solve-1
  []
  (let [lines (get-input! input)
        full-lines (map (fn [[[x1 y1] [x2 y2]]]
                          (cond
                            (= x1 x2) (into [] (map (fn [y] [x1 y])) (sorted-range y1 y2))
                            (= y1 y2) (into [] (map (fn [x] [x y1])) (sorted-range x1 x2))
                            :else []))
                        lines)]
    (->> full-lines
         (apply concat)
         (frequencies)
         (vals)
         (filter #(> % 1))
         (count))))

(defn zip
  [& args]
  (apply map vector args))

(defn solve-2
  []
  (let [lines (get-input! input)
        full-lines (map (fn [[[x1 y1] [x2 y2]]]
                          (cond
                            (= x1 x2) (into [] (map (fn [y] [x1 y])) (sorted-range y1 y2))
                            (= y1 y2) (into [] (map (fn [x] [x y1])) (sorted-range x1 x2))
                            :else (into [] (zip (sorted-range x1 x2)
                                                (sorted-range y1 y2)))))
                        lines)]
    (zip lines full-lines)
    (->> full-lines
         (apply concat)
         (frequencies)
         (vals)
         (filter #(> % 1))
         (count))))
