(ns aoc22.day1
  (:require [clojure.java.io :as io]))

(def input "./data/aoc22/day1.txt")

(defn section-seq
  ([line-parser]
   (section-seq line-parser conj (conj)))
  ([line-parser reducer]
   (section-seq line-parser reducer (reducer)))
  ([line-parser reducer init]
   (fn [xf]
     (let [state (volatile! init)
           last-input (volatile! nil)]
       (fn
         ([] (xf))
         ([result]
          (xf (xf result @state)))
         ([result input]
          (let [last-input# @last-input]
            (vreset! last-input input)
            (if (= input "")
              (if (= last-input# "")
                result
                (let [v @state]
                  (vreset! state init)
                  (xf result v)))
              (do (vswap! state reducer (line-parser input))
                  result)))))))))

#_(def partition-by-nil
  (comp (partition-by nil?)
        (remove #(->> % first nil?))))

(defn get-calories!
  [input]
  (with-open [reader (io/reader input)]
    (into [] (section-seq parse-long +) (line-seq reader))))

(defn solve-1
  []
  (->> (get-calories! input)
       (sort >)
       first))

(defn solve-2
  []
  (->> (get-calories! input)
       (sort >)
       (take 3)
       (reduce +)))
