(ns aoc24.day24
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day24.txt")

(defn parse-init
  [line]
  (let [[gate v] (clojure.string/split line #": ")]
    [gate (parse-long v)]))

(defn parse-gate
  [line]
  (let [[_ a op b r] (re-matches #"([a-z0-9]+) (XOR|OR|AND) ([a-z0-9]+) \-\> ([a-z0-9]+)" line)]
    [a op b r]))

(defn get-input!
  []
  (let [[init gates] (with-open [reader (io/reader input)]
                       (->> (line-seq reader)
                            (partition-by #{""})
                            (remove #(= % [""]))
                            (doall)))]
    [(->> init
          (map parse-init)
          (into {}))
     (->> gates
          (map parse-gate))]))

(defn run
  [m gates used]
  (let [m' (->> (keys m) (set))
        gates' (->> gates
                    (filter (fn [[a op b r]]
                              (and (not (contains? used [a op b r]))
                                   (contains? m' a)
                                   (contains? m' b)))))
        r (->> gates'
               (reduce (fn [m [a op b r]]
                         (case op
                           "AND" (assoc m r (bit-and (get m a) (get m b)))
                           "OR"  (assoc m r (bit-or (get m a) (get m b)))
                           "XOR" (assoc m r (bit-xor (get m a) (get m b)))))
                       m))]
    (if (empty? gates')
      r
      (recur r gates (into used gates')))))

(defn binary->long
  [s]
  (Long/parseLong s 2))

(defn solve-1
  []
  (let [[m gates] (get-input!)]
    (->> (run m gates #{})
         (sort-by first)
         (filter (fn [[k v]] (clojure.string/starts-with? k "z")))
         (map second)
         (reverse)
         (reduce str "")
         (binary->long))))
