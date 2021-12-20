(ns aoc21.day14
  (:require [clojure.java.io :as io]))

(def input "./data/aoc21/day14.txt")

(def to-num
  (into {} (map vector (set (clojure.string/split "SVCHKVFKCSHVFNBKKPOC" #"")) (range))))

(defn parse-pair
  [s]
  (let [[a b] (clojure.string/split s #" -> ")]
    [(mapv to-num (clojure.string/split a #"")) (to-num b)]))

(defn get-input!
  [input]
  (let [[input _ & pairs]
        (with-open [reader (io/reader input)]
          (doall (line-seq reader)))]
    [(mapv to-num (clojure.string/split input #""))
     (into {} (map parse-pair) pairs)]))

(defn step
  [pairs input]
  (persistent!
    (reduce (fn [acc pair]
              (let [match (get pairs pair)
                    [_ b] pair]
                (if match
                  (-> acc (conj! match) (conj! b))
                  (conj! acc b))))
            (transient [(first input)])
            (partition 2 1 input))))

(defn result
  [col]
  (let [xs (->> col
                frequencies
                (sort-by second >)
                (map second))]
    (- (first xs) (last xs))))

(defn result2
  [m]
  (let [xs (->> m
                (sort-by second >)
                (map second))]
    (- (first xs) (last xs))))

(defn solve-1
  []
  (let [[input pairs] (get-input! input)]
    (->> (iterate (partial step pairs) input)
         (drop 10)
         (take 1)
         (first)
         result)))

(def x
  (memoize
    (fn [pairs max i input]
      (if (= max i)
        (->> input (drop 1) frequencies)
        (->> input
             (partition 2 1)
             (map (fn [pair]
                    (let [m (get pairs pair)
                          [a b] pair]
                      (x pairs max (inc i) [a m b]))))
             (reduce (partial merge-with +) {}))))))

(defn solve-2
  []
  (let [[input pairs] (get-input! input)]
    (->> (merge-with + (x pairs 40 0 input)
                     (frequencies (take 1 input)))
         result2)))
