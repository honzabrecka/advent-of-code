(ns aoc21.day18
  (:require [clojure.java.io :as io]
            [clojure.zip :as z]))

(def input "./data/aoc21/day18.txt")

(defn find-node
  [move pred xs]
  (loop [node (move xs)]
    (when-not (or (nil? node) (z/end? node))
      (if (pred node)
        node
        (recur (move node))))))

(def find-right (partial find-node z/next))

(def find-left (partial find-node z/prev))

(defn simple?
  [xs]
  (number? (z/node xs)))

(defn explode?
  [node]
  (and (z/branch? node)
       (-> node z/path count (= 4))))

(defn explode
  [xs]
  (let [update-node
        (fn [find-next find-prev n xs]
          (if-some [node (find-next simple? xs)]
            (find-prev simple? (z/edit node + n))
            xs))
        [a b] (z/node xs)]
    (->> (z/replace xs 0)
         (update-node find-left find-right a)
         (update-node find-right find-left b))))

(defn action-explode
  [xs]
  (if-some [node (find-right explode? xs)]
    (explode node)
    xs))

(defn split?
  [xs]
  (and (simple? xs)
       (>= (z/node xs) 10)))

(defn split
  [node]
  (let [n (/ (z/node node) 2)]
    [(int (Math/floor n)) (int (Math/ceil n))]))

(defn action-split
  [xs]
  (if-some [node (find-right split? xs)]
    (z/replace node (split node))
    xs))

(defn simplify
  [xs]
  (let [xs' (-> xs z/vector-zip action-explode z/root)]
    (if (not= xs' xs)
      (recur xs')
      (let [xs'' (-> xs' z/vector-zip action-split z/root)]
        (if (not= xs'' xs')
          (recur xs'') xs'')))))

(defn magnitude
  [x]
  (if (number? x)
    (long x)
    (let [[a b] x]
      (+ (* 3 (magnitude a)) (* 2 (magnitude b))))))

(defn add
  [a b]
  [a b])

(defn get-input!
  [input]
  (with-open [reader (io/reader input)]
    (into []
          (map clojure.edn/read-string)
          (line-seq reader))))

(defn solve-1
  []
  (->> (get-input! input)
       (reduce (fn [acc xs]
                 (simplify (add acc xs))))
       (magnitude)))

(defn solve-2
  []
  (let [xs (get-input! input)]
    (->> (for [a xs b xs :when (not= a b)]
           [a b])
         (map (fn [[a b]] (-> (add a b) simplify magnitude)))
         (apply max))))
