(ns aoc18.day2
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing run-tests]]))

(def input "./data/aoc18/day2.txt")

(defn counts
  [ids n]
  (->> ids
       (map frequencies)
       (map vals)
       (map set)
       (filter #(some (partial = n) %))
       (count)))

(deftest counts-test
  (let [ids ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"]]
    (is (= (counts ids 2) 4))
    (is (= (counts ids 3) 3))))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [ids (line-seq reader)]
      (* (counts ids 2)
         (counts ids 3)))))

(deftest solve-1-test
  (with-redefs [line-seq (fn [_] ["abcdef"
                                  "bababc"
                                  "abbcde"
                                  "abcccd"
                                  "aabcdd"
                                  "abcdee"
                                  "ababab"])]
    (is (= (solve-1) 12))))

(defn find
  [pred col]
  (reduce (fn [nothing x]
            (if (pred x) (reduced x) nothing))
          nil col))

(deftest find-test
  (is (nil? (find true? [false false false])))
  (is (= (find #(= % 2) [0 1 2 3 4]) 2)))

(defn hamming-distance
  [a b]
  (if (= (count a) (count b))
    (->> (map vector a b)
         (filter #(apply not= %))
         (count))
    -1))

(deftest hamming-distance-test
  (is (= (hamming-distance "a" "ab") -1))
  (is (= (hamming-distance "fghij" "fguij") 1)))

(defn ordered-intersection
  [a b]
  (->> (map vector a b)
       (filter #(apply = %))
       (map first)
       (reduce str "")))

(deftest ordered-intersection-test
  (is (= (ordered-intersection "fghij" "fguij") "fgij")))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [ids (into [] (line-seq reader))]
      (->> ids
           (map (fn [a]
                  (if-let [b (find #(= (hamming-distance a %) 1) ids)]
                    [a b]
                    nil)))
           (find (complement nil?))
           (apply ordered-intersection)))))

(deftest solve-2-test
  (with-redefs [line-seq (fn [_] ["abcde"
                                  "fghij"
                                  "klmno"
                                  "pqrst"
                                  "fguij"
                                  "axcye"
                                  "wvxyz"])]
    (is (= (solve-2) "fgij"))))
