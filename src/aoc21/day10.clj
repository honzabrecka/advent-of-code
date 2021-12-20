(ns aoc21.day10
  (:require [clojure.java.io :as io]))

(def input-test "{([(<{}[<>[]}>{[]{[(<()>")

(def input "./data/aoc21/day10.txt")

(defn parse-line
  [s]
  (keep-indexed vector s))

(def tags
  {\( \)
   \{ \}
   \[ \]
   \< \>})

(defn validate
  [program]
  (let [ends (set (vals tags))
        starts (set (keys tags))]
    (reduce (fn [acc [i x]]
              (cond
                (contains? starts x)
                (concat acc [x])
                (contains? ends x)
                (if (= (get tags (last acc)) x)
                  (butlast acc)
                  (reduced [:error :wrong-closing-tag acc i x (get tags (last acc))]))
                :else
                (reduced [:error :unexpected-char acc i x])))
            []
            program)))

(def points1
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (map validate)
         (filter #(= (take 1 %) [:error]))
         (map (fn [[_ _ _ _ actual expected]]
                actual))
         (map points1)
         (reduce +))))

(def points2
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn middle
  [col]
  (nth col (int (/ (count col) 2))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (map validate)
         (remove #(= (take 1 %) [:error]))
         (map (fn [program]
                (->> (reverse program)
                     (map tags)
                     (map points2)
                     (reduce (fn [acc p] (+ (* acc 5) p)) 0))))
         sort
         middle)))
