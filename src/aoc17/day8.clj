(ns aoc17.day8
  (:require [clojure.java.io :as io]))

(def input "./data/aoc17/day8.txt")

(def ops
  {"inc" +
   "dec" -})

(def comps
  {">"  >
   "<"  <
   ">=" >=
   "<=" <=
   "==" =
   "!=" not=})

(defn parse-line
  [s]
  (let [[_ target op n x comp v] (re-matches #"^([a-z]+) (dec|inc) ([\d\-]+) if ([a-z]+) ([><=\!]{1,2}) ([\d\-]+)$" s)]
    {:target target
     :op     (ops op)
     :n      (parse-long n)
     :x      x
     :comp   (comps comp)
     :v      (parse-long v)}))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (reduce (fn [acc {:keys [target op n x comp v]}]
                   (if (comp (get acc x 0) v)
                     (update acc target #(op (or % 0) n))
                     acc)) {})
         vals
         (apply max))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (reduce (fn [[highest acc] {:keys [target op n x comp v]}]
                   (if (comp (get acc x 0) v)
                     (let [acc# (update acc target #(op (or % 0) n))]
                       [(max highest (get acc# target)) acc#])
                     [highest acc])) [0 {}])
         first)))
