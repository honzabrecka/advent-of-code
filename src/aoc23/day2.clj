(ns aoc23.day2
  (:require [clojure.java.io :as io]))

(def input "./data/aoc23/day2.txt")

(defn parse-reveal
  [cube]
  (let [[count color] (clojure.string/split cube #" ")]
    [(keyword color) (parse-long count)]))

(defn parse-line
  [line]
  (let [[game reveals] (clojure.string/split line #": ")
        reveals (clojure.string/split reveals #"[,;] ")]
    [(-> (clojure.string/split game #" ")
         second
         parse-long)
     (map parse-reveal reveals)]))

(def max-cube-per-game
  {:red   12
   :green 13
   :blue  14})

(defn possible-game?
  [[color count]]
  (>= (- (get max-cube-per-game color) count) 0))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (filter (fn [[_ reveals]]
                   (every? possible-game? reveals)))
         (map first)
         (reduce +))))

(def empty-cube
  {:red   0
   :green 0
   :blue  0})

(defn greater
  [a b]
  (if (> a b) a b))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (map (fn [[_ reveals]]
                (reduce (fn [acc [color count]]
                          (update acc color greater count))
                        empty-cube reveals)))
         (map #(apply * (vals %)))
         (reduce +))))
