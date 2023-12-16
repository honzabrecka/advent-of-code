(ns aoc23.day16
  (:require [clojure.java.io :as io]))

(def input "./data/aoc23/day16.txt")

(defn get-grid!
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (mapv vec)
         doall)))

(def moves
  {:r [1 0]
   :l [-1 0]
   :u [0 -1]
   :d [0 1]})

(defn move
  [[[x y] d]]
  (let [[dx dy] (moves d)]
    [[(+ dx x) (+ dy y)] d]))

(defn change-direction
  [grid beam]
  (let [[p d] beam
        [x y] p
        v (get-in grid [y x])]
    (cond
      (= v \.)
      [(move beam)]
      (and (= v \-)
           (or (= d :r)
               (= d :l)))
      [(move beam)]
      (and (= v \|)
           (or (= d :u)
               (= d :d)))
      [(move beam)]
      (and (= v \-)
           (or (= d :u)
               (= d :d)))
      [(move [p :l]) (move [p :r])]
      (and (= v \|)
           (or (= d :l)
               (= d :r)))
      [(move [p :u]) (move [p :d])]
      (and (= v \\)
           (= d :r))
      [(move [p :d])]
      (and (= v \\)
           (= d :u))
      [(move [p :l])]
      (and (= v \\)
           (= d :l))
      [(move [p :u])]
      (and (= v \\)
           (= d :d))
      [(move [p :r])]
      (and (= v \/)
           (= d :l))
      [(move [p :d])]
      (and (= v \/)
           (= d :u))
      [(move [p :r])]
      (and (= v \/)
           (= d :r))
      [(move [p :u])]
      (and (= v \/)
           (= d :d))
      [(move [p :l])]
      :else (throw (Error. (str "wrong! " v " " d))))))

(defn in-grid?
  [w h [[x y] _]]
  (and (>= x 0)
       (>= y 0)
       (< x w)
       (< y h)))

(defn in-cycle?
  [energized beam]
  (contains? energized beam))

(defn walk
  [grid w h start]
  (loop [beams [start]
         energized #{start}]
    (let [beams' (->> beams
                      (mapcat #(change-direction grid %))
                      (filter #(and (in-grid? w h %)
                                    (not (in-cycle? energized %)))))]
      (if (zero? (count beams'))
        (count (into #{} (map first energized)))
        (recur beams' (into energized beams'))))))

(defn solve-1
  []
  (let [grid (get-grid!)
        w (count (get grid 0))
        h (count grid)
        start [[0 0] :r]]
    (walk grid w h start)))

(defn starts
  [w h]
  (concat (map (fn [x] [[x 0] :d]) (range w))
          (map (fn [x] [[x (dec h)] :u]) (range w))
          (map (fn [y] [[0 y] :r]) (range h))
          (map (fn [y] [[(dec w) y] :l]) (range h))))

; slow, but does the job
(defn solve-2
  []
  (let [grid (get-grid!)
        w (count (get grid 0))
        h (count grid)]
    (->> (starts w h)
         (pmap #(walk grid w h %))
         (sort >)
         first)))
