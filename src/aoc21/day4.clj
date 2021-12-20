(ns aoc21.day4
  (:require [clojure.java.io :as io]))

(def input "./data/aoc21/day4.txt")

(defn parse-board
  [[_ & rows]]
  (mapv (fn [row]
          (mapv parse-long (clojure.string/split (clojure.string/trim row) #"[ ]+")))
        rows))

(defn get-input!
  [input]
  (with-open [reader (io/reader input)]
    (let [[numbers & boards] (doall (line-seq reader))]
      {:numbers (map parse-long (clojure.string/split numbers #","))
       :boards  (map parse-board (partition 6 boards))})))

(defn rotate-matrix
  [matrix]
  (apply map vector matrix))

(defn winning-board?
  [played board]
  (let [rotated (rotate-matrix board)]
    (some (fn [row-or-column]
            (when (clojure.set/subset? (set row-or-column) played)
              [board row-or-column]))
          (concat board rotated))))

(defn play-first
  [{:keys [numbers boards]}]
  (loop [[n & ns] numbers
         played #{}]
    (let [played (conj played n)
          winning (some #(winning-board? played %) boards)]
      (if winning
        [played n winning]
        (recur ns played)))))

(defn solve-1
  []
  (let [[played last-played [winning-board winning-row-or-column]]
        (play-first (get-input! input))
        board# (set (flatten winning-board))]
    (* last-played
       (reduce + (clojure.set/difference board# played)))))

(defn play-all
  [{:keys [numbers boards]}]
  (loop [boards boards
         [n & ns] numbers
         played #{}
         wins []]
    (let [played (conj played n)
          winning (remove nil? (map #(winning-board? played %) boards))
          winning? (> (count winning) 0)
          boards# (if winning?
                    (remove (fn [board]
                              (let [winning# (set (map first winning))]
                                (contains? winning# board))) boards)
                    boards)
          wins# (if winning?
                  (concat wins (map (fn [win] [played n win]) winning))
                  wins)]
      (if (nil? ns)
        wins#
        (recur boards# ns played wins#)))))

(defn solve-2
  []
  (let [[played last-played [winning-board winning-row-or-column]]
        (last (play-all (get-input! input)))
        board# (set (flatten winning-board))]
    (* last-played
       (reduce + (clojure.set/difference board# played)))))
