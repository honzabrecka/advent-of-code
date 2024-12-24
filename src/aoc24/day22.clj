(ns aoc24.day22
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day22.test.txt")

#_(defn lazy-pad
  [pad seq]
  (if (empty? seq)
    (repeat pad)
    (lazy-seq (cons (first seq) (lazy-pad pad (rest seq))))))

#_(defn binary->decimal
  [s]
  (Long/parseLong (str s) 2))

#_(defn xor
  [a b]
  (let [a' (reverse (Long/toBinaryString a))
        b' (reverse (Long/toBinaryString b))
        l (max (count a') (count b'))]
    (->> (map vector
              (->> a' (lazy-pad \0) (take l))
              (->> b' (lazy-pad \0) (take l)))
         (reverse)
         (map (fn [[a b]]
                (or (and (= a \1) (= b \0))
                    (and (= a \0) (= b \1)))))
         (map #(if (true? %) \1 \0))
         (reduce str)
         binary->decimal)))

(def mix bit-xor)

(defn prune
  [n]
  (mod n 16777216))

(def next-secret
  (memoize
    (fn [secret]
      (letfn [(a [secret]
                (-> secret
                    (* 64)
                    (mix secret)
                    (prune)))
              (b [secret]
                (-> secret
                    (/ 32)
                    (long)
                    (mix secret)
                    (prune)))
              (c [secret]
                (-> secret
                    (* 2048)
                    (mix secret)
                    (prune)))]
        (-> secret a b c)))))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map (fn [init-secret]
                (nth (iterate next-secret (parse-long init-secret)) 2000)))
         (reduce +))))

(defn last-digit
  [n]
  (mod n 10))

(defn prices
  [n init-secret]
  (let [prices (->> (iterate next-secret (parse-long init-secret))
                    (map last-digit)
                    (take n))]
    (->> (map vector (drop 1 prices) prices)
         (map (fn [[a b]]
                ; [last-digit (price), diff]
                [a (- a b)])))))

(defn solve-2
  []
  (let [groups (with-open [reader (io/reader input)]
                 (->> (line-seq reader)
                      (map (fn [init-secret]
                             (->> init-secret
                                  (prices 2000)
                                  (partition 4 1)
                                  (group-by #(map second %)))))
                      (into [])))]
    (->> (apply clojure.set/union (->> groups (map keys) (map set)))
         (map (fn [k]
                (->> groups
                     (map (fn [g]
                            (if-let [v (get g k)]
                              (->> v first (map first) last)
                              0)))
                     (reduce +))))
         (sort >)
         (first))))
