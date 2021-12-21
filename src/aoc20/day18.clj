(ns aoc20.day18
  (:require [clojure.java.io :as io]))

(def input "./data/aoc20/day18.txt")

(defn parse
  [row]
  (->> (clojure.string/split row #"x")
       (map #(Integer/parseInt %))))

(defn parse
  [row]
  (clojure.edn/read-string (str "(" row ")")))

(def op
  {(symbol "+") +
   (symbol "*") *})

(defn precedence
  [form]
  (if (> (count form) 3)
    (let [[a o b & rest] form]
      (if (= o (symbol "*"))
        (concat [a o (precedence (concat [b] rest))])
        (precedence (concat [[a o b]] rest))))
    form))

(defn evaluate
  [precedence form]
  (cond
    (and (coll? form) (>= (count form) 3))
    (let [[a o b & rest] (precedence form)
          r ((get op o) (evaluate precedence a) (evaluate precedence b))]
      (if (nil? rest)
        r
        (recur precedence (concat [r] rest))))
    (and (coll? form) (= (count form) 1))
    (recur precedence (first form))
    :else
    form))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (transduce (comp (map parse)
                     (map #(evaluate identity %)))
               +
               (line-seq reader))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (transduce (comp (map parse)
                     (map #(evaluate precedence %)))
               +
               (line-seq reader))))
