(ns aoc23.day10
  (:require [clojure.java.io :as io]))

(def input "./data/aoc23/day10.txt")

(defn start?
  [grid [x y]]
  (= (get-in grid [y x]) \S))

(defn find-start
  [grid]
  (let [w (count (get grid 0))
        h (count grid)]
    (->> (for [x (range w)
               y (range h)
               :when (start? grid [x y])]
           [x y])
         first)))

(def diffs
  {:n [0 -1]
   :s [0 1]
   :w [-1 0]
   :e [1 0]})

(defn apply-diff
  [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn get-in-grid
  [grid [x y]]
  (get-in grid [y x]))

(def possible-directions
  {:n {\| :n
       \7 :w
       \F :e}
   :s {\| :s
       \J :w
       \L :e}
   :w {\- :w
       \L :n
       \F :s}
   :e {\- :e
       \7 :s
       \J :n}})

(defn go
  [grid position path direction]
  (let [diff (get diffs direction)
        position' (apply-diff position diff)
        val (get-in-grid grid position')
        by-val (get possible-directions direction)
        direction' (get by-val val)]
    (cond
      (start? grid position') path
      (nil? direction') nil
      :else (recur grid position' (conj path [position' val direction']) direction'))))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [grid (->> (line-seq reader) (map vec) (into []))
          start (find-start grid)]
      (->> [:n :s :w :e]
           (map #(go grid start [[start \S %]] %))
           (remove nil?)
           (map (fn [path] (int (/ (count path) 2))))
           (apply max)))))

(defn grab
  [grid circle# [position _ direction]]
  (loop [[p & ps] [position]
         seen #{}
         inner? true]
    (if (nil? p)
      [seen inner?]
      (let [ps' (->> (get {:s [[-1 0] [-1 1] [0 1]]
                           :n [[1 0] [1 -1] [0 -1]]
                           :e [[1 0] [1 1] [0 1]]
                           :w [[-1 0] [0 -1] [-1 -1]]}
                          direction)
                     (map #(apply-diff p %)))
            inner?' (some #(nil? (get-in-grid grid %)) ps')
            safe-ps (->> ps'
                         (remove (fn [p']
                                   (or (nil? (get-in-grid grid p'))
                                       (contains? seen p')
                                       (contains? circle# p')))))]
        (recur (concat ps safe-ps)
               (into seen safe-ps)
               (and inner? (nil? inner?')))))))

(defn flood
  [grid circle]
  (let [circle# (into #{} (map first) circle)]
    (->> circle (pmap #(grab grid circle# %)))))

(defn merge-floods
  [floods]
  (->> floods
       (reduce (fn [[flooded inner?] [flooded' inner?']]
                 [(into flooded flooded')
                  (and inner? inner?')])
               [#{} true])))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [grid (->> (line-seq reader) (map vec) (into []))
          start (find-start grid)]
      (->> [:n :s :w :e]
           (pmap #(go grid start [[start \S %]] %))
           (remove nil?)
           (map #(flood grid %))
           (map merge-floods)
           (filter #(true? (second %)))
           ffirst
           count))))
