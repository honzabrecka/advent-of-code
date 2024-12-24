(ns aoc24.day12
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day12.test.txt")

(defn get-grid!
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (keep-indexed (fn [y v] (vec (keep-indexed (fn [x c] [[(* 2 x) (* 2 y)] c]) v))))
         (into []))))

(defn rotate
  [matrix]
  (apply map vector matrix))

(defn edges
  [area]
  (into #{}
        (mapcat (fn [[x y]]
                  (->> [[-1 0] [0 -1] [1 0] [0 1]]
                       (filter (fn [[dx dy]]
                                   (not (contains? area [(+ x (* 2 dx)) (+ y (* 2 dy))]))))
                       (map (fn [[dx dy]]
                              [(+ x dx) (+ y dy)])))))
        area))

(defn vertices
  [area]
  (into #{}
        (mapcat (fn [[x y]]
                  (->> [[-1 -1] [1 -1] [-1 1] [1 1]]
                       (map (fn [[dx dy]]
                              [(+ x dx) (+ y dy) dx dy]))
                       (filter (fn [[x y dx dy]]
                                 (or (->> [[-1 -1] [1 -1] [-1 1] [1 1]]
                                          (filter (fn [[dx dy]]
                                                    (contains? area [(+ x dx) (+ y dy)])))
                                          (count)
                                          (contains? #{1 3}))
                                     (contains? area [(+ x (* 1 dx)) (+ y (* 1 dy))]))))
                       (map (fn [[x y _dx _dy]] [x y])))))
        area))



(defn partition-line
  [line]
  (->> line
       (partition-by second)
       (mapcat (fn [group]
              (let [s (into #{} (map first) group)]
                (map (fn [s'] [s' s]) s))))))

(defn get-area
  ([by-pos p]
   (get-area by-pos [p] #{p}))
  ([by-pos ps area]
   (let [ps' (->> ps
                  (mapcat #(get by-pos %))
                  (remove #(contains? area %)))]
     (if (empty? ps')
       area
       (recur by-pos ps' (reduce conj area ps'))))))

(defn get-by-pos
  [grid]
  (let [horizontal (into {} (mapcat partition-line) grid)
        vertical (into {} (mapcat partition-line) (rotate grid))
        by-pos (merge-with clojure.set/union horizontal vertical)]
    by-pos))

(defn areas
  [grid]
  (let [by-pos (get-by-pos grid)]
    (->> by-pos
         (keys)
         (map #(get-area by-pos %))
         (into #{}))))

(defn c
  [areas]
  (->> areas
       (map (fn [area]
              [(count area) (count (edges area))]))
       (map #(apply * %))
       (reduce +)))

(defn c
  [areas]
  (->> areas
       (map (fn [area]
              [(count area) (count
                              (vertices area)
                              #_(clojure.set/difference
                                     (vertices area)
                                     (edges area)))]))
       (map #(apply * %))
       (reduce +)))

(defn solve-1
  []
  (->> (get-grid!)
       ))

;+-+-+-+
;|E|X|X|
;+-+-+-+
;|X|E|X|
;+-+-+-+
;|X|X|X|
;+-+-+-+