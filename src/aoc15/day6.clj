(ns aoc15.day6
  (:require [clojure.java.io :as io]))

(def input "./data/aoc15/day6.txt")

(defn parse-line
  [s]
  (let [[_ action x y w h] (re-matches #"^(toggle|turn on|turn off) ([\d]+),([\d]+) through ([\d]+),([\d]+)$" s)]
    {:action action
     :x      (parse-long x)
     :y      (parse-long y)
     :w      (parse-long w)
     :h      (parse-long h)}))

(defn get-input!
  [input]
  (with-open [reader (io/reader input)]
    (into []
          (map parse-line)
          (line-seq reader))))

(def actions
  {"toggle"   (fn [state] (not state))
   "turn on"  (fn [_state] true)
   "turn off" (fn [_state] false)})

(defn area
  [x y w h]
  (for [x' (range x (inc w))
        y' (range y (inc h))]
    [x' y']))

(defn solve-1
  []
  (let [instructions (get-input! input)
        lights (reduce (fn [lights {:keys [action x y w h]}]
                         (reduce (fn [lights pos]
                                   (update lights pos (actions action)))
                                 lights (area x y w h)))
                       {} instructions)]
    (transduce (comp (map second) (filter true?) (map (fn [_] 1))) + 0 lights)))

(def actions-2
  {"toggle"   (fn [state] (+ state 2))
   "turn on"  (fn [state] (inc state))
   "turn off" (fn [state] (let [state' (dec state)]
                            (if (< state' 0) 0 state')))})

(defn solve-2
  []
  (let [instructions (get-input! input)
        lights (reduce (fn [lights {:keys [action x y w h]}]
                         (reduce (fn [lights pos]
                                   (update lights pos (fnil (actions-2 action) 0)))
                                 lights (area x y w h)))
                       {} instructions)]
    (transduce (map second) + 0 lights)))
