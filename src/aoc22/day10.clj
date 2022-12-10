(ns aoc22.day10
  (:require [clojure.java.io :as io]))

(def input "./data/aoc22/day10.txt")

(defn parse-line
  [s]
  (let [[_ instruction _ x] (re-matches #"^(noop|addx)( ([\-\d]+))?$" s)]
    (if (= instruction "noop")
      [{:type :noop}]
      [{:type :noop} {:type :addx :value (parse-long x)}])))

(def instructions
  {:noop (fn [x _] x)
   :addx (fn [x v] (+ x v))})

(defn signal
  ([program]
   (let [x 1]
     (cons x (signal x (rest program)))))
  ([x [{:keys [type value]} & program]]
   (lazy-seq (cons x (signal ((get instructions type) x value) program)))))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [cycles (range 20 (inc 220) 40)
          program (cycle (into [] (mapcat parse-line) (line-seq reader)))
          signal# (signal program)]

      (->> cycles
           (map #(* % (nth signal# (dec %))))
           (reduce +)))))

(defn draw-line
  [line]
  (->> (keep-indexed vector line)
       (map (fn [[i v]]
              (let [v# (dec v)]
                (if (contains? (set (range v# (+ v# 3))) i) 1 0))))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [program (cycle (into [] (mapcat parse-line) (line-seq reader)))
          signal# (take 240 (signal program))]
      (->> (partition 40 signal#)
           (map draw-line)))))
