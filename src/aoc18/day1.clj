(ns aoc18.day1
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing run-tests]]))

(def input "./data/aoc18/day1.txt.txt")

(defn parse-change
  [change]
  (let [operation (subs change 0 1)
        n (subs change 1)]
    (* (if (= "-" operation) -1 1)
       (Integer/parseInt n))))

(deftest parse-change-test
  (is (= (parse-change "-3") -3)))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-change)
         (reduce +))))

(deftest solve-1-test
  (with-redefs [line-seq (fn [_] ["+1" "+1" "-2"])]
    (is (= (solve-1) 0))))

(defn first-duplicated
  [xs]
  (reduce (fn [seen x]
            (if (contains? seen x)
              (reduced x)
              (conj seen x)))
          #{} xs))

(deftest first-duplicated-test
  (is (= (first-duplicated [0 1 2]) #{0 1 2}))
  (is (= (first-duplicated [0 1 2 1 3 4]) 1)))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-change)
         (cycle)
         (reductions +)
         (first-duplicated))))

(deftest solve-2-test
  (with-redefs [line-seq (fn [_] ["-6" "+3" "+8" "+5" "-6"])]
    (is (= (solve-2) 5))))
