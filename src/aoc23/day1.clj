(ns aoc23.day1
  (:require [clojure.java.io :as io]))

(def input "./data/aoc23/day1.txt")

(defn get-ints
  [str]
  (->> (clojure.string/split str #"")
       (map parse-long)
       (remove nil?)))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map get-ints)
         (map (juxt first last))
         (reduce (fn [acc [a b]]
                   (+ acc (* a 10) b))
                 0))))

(defn get-ints2
  [value]
  (let [words {"one"   1
               "two"   2
               "three" 3
               "four"  4
               "five"  5
               "six"   6
               "seven" 7
               "eight" 8
               "nine"  9
               "1"     1
               "2"     2
               "3"     3
               "4"     4
               "5"     5
               "6"     6
               "7"     7
               "8"     8
               "9"     9}
        words' (->> words keys)]
    (loop [rest value
           result []]
      (if (= rest "")
        result
        (let [[found] (filter #(clojure.string/starts-with? rest %) words')]
          (recur
            (subs rest 1)
            (if found
              (conj result (get words found))
              result)))))))

;"nineight" -> [9, 8]
;(calibrate2 "5fasnineight6")
;=> [5 9 8 6]

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map get-ints2)
         (map (juxt first last))
         (reduce (fn [acc [a b]]
                   (+ acc (* a 10) b))
                 0))))
