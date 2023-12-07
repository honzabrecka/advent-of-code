(ns aoc23.day7
  (:require [clojure.java.io :as io]))

(def input "./data/aoc23/day7.txt")

(def hand-types
  {[5]         [:five-of-a-kind 6]
   [4 1]       [:four-of-a-kind 5]
   [3 2]       [:full-house 4]
   [3 1 1]     [:three-of-a-kind 3]
   [2 2 1]     [:two-pair 2]
   [2 1 1 1]   [:one-pair 1]
   [1 1 1 1 1] [:high-card 0]})

(defn simple-hand-type
  [hand]
  (get hand-types
       (->> (frequencies hand)
            (vals)
            (sort >))))

(defn parse-line
  [line]
  (let [[hand bid] (clojure.string/split line #" ")]
    [(vec hand) (parse-long bid)]))

(defn compare-hands
  [card-order hand-type [a _] [b _]]
  (let [a' (hand-type a)
        b' (hand-type b)]
    (if (= a' b')
      (->> (map vector a b)
           (reduce (fn [_ [a b]]
                     (let [a' (get card-order a)
                           b' (get card-order b)]
                       (when (not= a' b')
                         (reduced (< a' b')))))
                   nil))
      (< (second a') (second b')))))

(defn solve-1
  []
  (let [card-order {\A 14
                    \K 13
                    \Q 12
                    \J 11
                    \T 10
                    \9 9
                    \8 8
                    \7 7
                    \6 6
                    \5 5
                    \4 4
                    \3 3
                    \2 2}]
    (with-open [reader (io/reader input)]
      (->> (line-seq reader)
           (map parse-line)
           (sort #(compare-hands card-order simple-hand-type %1 %2))
           (keep-indexed vector)
           (reduce (fn [acc [i [_ bid]]]
                     (+ acc (* (inc i) bid))) 0)))))

(def safe+ (fnil + 0 0))

(defn joker-hand-type
  [hand]
  (let [fqs (frequencies hand)
        fqs' (dissoc fqs \J)
        [highest & rest] (->> fqs' vals (sort >))]
    (get hand-types
         (conj rest (safe+ highest (get fqs \J))))))

(defn solve-2
  []
  (let [card-order {\A 14
                    \K 13
                    \Q 12
                    \T 10
                    \9 9
                    \8 8
                    \7 7
                    \6 6
                    \5 5
                    \4 4
                    \3 3
                    \2 2
                    \J 1}]
    (with-open [reader (io/reader input)]
      (->> (line-seq reader)
           (map parse-line)
           (sort #(compare-hands card-order joker-hand-type %1 %2))
           (keep-indexed vector)
           (reduce (fn [acc [i [_ bid]]]
                     (+ acc (* (inc i) bid))) 0)))))
