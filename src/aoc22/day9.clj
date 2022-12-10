(ns aoc22.day9
  (:require [clojure.java.io :as io]))

(def input "./data/aoc22/day9.txt")

(defn parse-line
  [s]
  (let [[_ direction count] (re-matches #"(R|L|U|D) ([\d]+)" s)]
    (map (fn [_] (keyword direction)) (range (parse-long count)))))

(def ds
  {:L [-1 0]
   :R [1 0]
   :U [0 1]
   :D [0 -1]})

(defn move-head
  [[x y] direction]
  (let [[dx dy] (get ds direction)]
    [(+ x dx) (+ y dy)]))

(defn tail-fn
  [n]
  (cond
    (zero? n) identity
    (neg? n) inc
    (pos? n) dec))

(defn move-tail
  [[x y] [cx cy]]
  (let [x# (- x cx)
        y# (- y cy)]
    (if
      (or (contains? #{-2 2} x#)
          (contains? #{-2 2} y#))
      [((tail-fn x#) x) ((tail-fn y#) y)]
      [x y])))

(defn solve-1
  []
  (let [head (with-open [reader (io/reader input)]
               (->> (line-seq reader)
                    (mapcat parse-line)
                    (reductions move-head [0 0])
                    doall))]
    (->> head
         (reductions move-tail)
         set
         count)))

(defn solve-2
  []
  (let [head (with-open [reader (io/reader input)]
               (->> (line-seq reader)
                    (mapcat parse-line)
                    (reductions move-head [0 0])
                    doall))
        tails (iterate (partial reductions move-tail) head)]
    (-> tails (nth 9) set count)))
