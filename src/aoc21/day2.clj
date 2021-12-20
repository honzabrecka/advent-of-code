(ns aoc21.day2
  (:require [clojure.java.io :as io]))

(def input "./data/aoc21/day2.txt")

(defn parse-line
  [s]
  (let [[direction step] (clojure.string/split s #" ")]
    [direction (parse-long step)]))

(defn multiply-result
  [{:keys [horizontal depth]}]
  (* horizontal depth))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (reduce (fn [acc [direction step]]
                   (case direction
                     "forward" (update acc :horizontal #(+ % step))
                     "down" (update acc :depth #(+ % step))
                     "up" (update acc :depth #(- % step))))
                 {:horizontal 0 :depth 0})
         multiply-result)))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (reduce (fn [acc [direction step]]
                   (case direction
                     "forward" (-> acc
                                   (update :horizontal #(+ % step))
                                   (update :depth #(+ % (* step (:aim acc)))))
                     "down" (update acc :aim #(+ % step))
                     "up" (update acc :aim #(- % step))))
                 {:horizontal 0 :depth 0 :aim 0})
         multiply-result)))
