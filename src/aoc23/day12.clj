(ns aoc23.day12
  (:require [clojure.java.io :as io]))

(def input "./data/aoc23/day12.txt")

(defn parse-line
  [line]
  (let [[row groups] (clojure.string/split line #" ")]
    [(vec row)
     (map parse-long (clojure.string/split groups #","))]))

(defn damaged?
  [c]
  (= c \#))

(defn operational?
  [c]
  (= c \.))

(defn unknown?
  [c]
  (= c \?))

(declare arrangements')

(defn arrangements-inner
  [row groups]
  (let [[g & gs] groups
        [a b] (split-at g row)]
    (if (some operational? a)
      0
      (cond
        (not= (count a) g) 0
        (zero? (count b)) (arrangements' b gs)
        (damaged? (first b)) 0
        (unknown? (first b)) (arrangements' (rest b) gs)
        :else (arrangements' b gs)))))

(defn arrangements
  [row groups]
  (let [[x & xs] row]
    (cond
      (nil? x) (if (zero? (count groups)) 1 0)
      (nil? groups) (if (some damaged? row) 0 1)
      (operational? x) (arrangements' xs groups)
      (damaged? x) (arrangements-inner row groups)
      (unknown? x) (+ (arrangements' xs groups)
                      (arrangements-inner row groups)))))

(def arrangements' (memoize arrangements))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (pmap #(apply arrangements %))
         (reduce +))))

(defn unfold
  [n [row groups]]
  [(apply concat (interpose [\?] (repeat n row)))
   (apply concat (repeat n groups))])

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (map #(unfold 5 %))
         (pmap #(apply arrangements %))
         (reduce +))))
