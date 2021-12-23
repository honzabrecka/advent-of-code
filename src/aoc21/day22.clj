(ns aoc21.day22
  (:require [clojure.java.io :as io]))

(def input "./data/aoc21/day22.txt")

(defn parse-ranges
  [s]
  (into []
        (comp (map #(clojure.string/split % #"="))
              (map (fn [[_ s]]
                     (sort (map parse-long (clojure.string/split s #"\.\."))))))
        (clojure.string/split s #",")))

(defn limit-ranges
  [min-from max-to ranges]
  (into []
        (comp (remove (fn [[from to]]
                        (or (> from max-to)
                            (< to min-from))))
              (map (fn [[from to]]
                     [(max min-from from)
                      (min max-to to)])))
        ranges))

(defn parse-line
  [s]
  (let [[op rest] (clojure.string/split s #" ")]
    [(keyword op)
     (parse-ranges rest)]))

#_(def op
  {:on  clojure.set/union
   :off clojure.set/difference})

(def op
    {:on  conj!
     :off disj!})

(defn range'
  [[a b]]
  (range a (inc b)))

(defn selections
  [[x-range y-range z-range]]
  (for [x (range' x-range)
        y (range' y-range)
        z (range' z-range)]
    (list x y z)))

(defn reducer
  [acc [k s]]
  (reduce (op k) acc s))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (count
      (persistent!
        (transduce (comp (map parse-line)
                         (map (fn [[op ranges]]
                                [op (limit-ranges -50 50 ranges)]))
                         (remove (fn [[op ranges]]
                                   (not= (count ranges) 3)))
                         (map (fn [[op ranges]]
                                [op (selections ranges)])))
                   (completing reducer)
                   (transient #{})
                   (line-seq reader))))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (count
      (persistent!
        (transduce (comp (map parse-line)
                         #_(map (fn [[op ranges]]
                                  [op (limit-ranges -50 50 ranges)]))
                         #_(remove (fn [[op ranges]]
                                     (not= (count ranges) 3)))
                         (map (fn [[op ranges]]
                                [op (fn [] (selections ranges))])))
                   (completing reducer)
                   (transient #{})
                   (line-seq reader))))))
