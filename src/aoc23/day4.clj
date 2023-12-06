(ns aoc23.day4
  (:require [clojure.java.io :as io]))

(def input "./data/aoc23/day4.txt")

(defn parse-line
  [line]
  (let [[meta rest] (clojure.string/split line #":")
        [_ id] (clojure.string/split meta #"[ ]+")
        [winning my] (clojure.string/split rest #" \| ")]
    {:id      id
     :winning (drop 1 (map parse-long (clojure.string/split winning #"[ ]+")))
     :my      (map parse-long (clojure.string/split my #"[ ]+"))}))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (map (fn [{:keys [winning my]}]
                (let [c (count (clojure.set/intersection (set winning) (set my)))]
                  (case c
                    0 0
                    1 1
                    (long (Math/pow 2 (dec c)))))))
         (reduce +))))

(defn solve-2'
  [cards]
  (loop [[x & xs] cards
         index {}]
    (if (nil? x)
      (+ (count cards) (reduce + (vals index)))
      (let [{:keys [id winning my]} x
            c (count (clojure.set/intersection (set winning) (set my)))
            c' (inc (get index id 0))
            index' (->> xs
                        (take c)
                        (map :id)
                        (reduce (fn [acc id']
                                  (update acc id' (fnil + 0) c')) index))]
        (recur xs index')))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         solve-2')))
