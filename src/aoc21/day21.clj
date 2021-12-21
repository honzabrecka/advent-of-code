(ns aoc21.day21
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def input "./data/aoc21/day21.txt")

(defn parse-line
  [s]
  (let [[_ player position] (re-matches #"Player (\d) starting position: (\d+)" s)]
    [(parse-long player)
     (parse-long position)]))

(defn get-starts!
  [input]
  (with-open [reader (io/reader input)]
    (into {}
          (map parse-line)
          (line-seq reader))))

(defn deterministic-dice
  []
  (cycle (range 1 (inc 100))))

(defn move
  [rolls position]
  (let [n (mod (reduce + position rolls) 10)]
    (if (zero? n) 10 n)))

(defn play
  [dice players]
  (let [n 3
        rolls (take n dice)
        [current-player & rest-players] players
        next-position (move rolls (:position current-player))
        next-score (+ (:score current-player) next-position)
        next-rolls (+ (:rolls current-player) n)
        next-player (-> current-player
                            (assoc :score next-score
                                   :position next-position
                                   :rolls next-rolls))]
    (if (>= next-score 1000)
      (concat [next-player] rest-players)
      (recur (drop n dice) (concat rest-players [next-player])))))

(defn player
  [id init-position]
  {:id       id
   :position init-position
   :score    0
   :rolls    0})

(defn result
  [players]
  (* (->> players (map :rolls) (reduce +))
     (:score (last players))))

(defn solve-1
  []
  (let [starts (get-starts! input)
        dice (deterministic-dice)
        players [(player 1 (get starts 1)) (player 2 (get starts 2))]]
    (result (play dice players))))

(def xs (combo/selections [1 2 3] 3))

(def play2
  (memoize
    (fn [rolls players]
      (let [[current-player & rest-players] players
            next-position (move rolls (:position current-player))
            next-score (+ (:score current-player) next-position)
            next-rolls (+ (:rolls current-player) (count rolls))
            next-player (-> current-player
                                (assoc :score next-score
                                       :position next-position
                                       :rolls next-rolls))]
        (if (>= next-score 21)
          {(:id next-player) 1}
          (->> (map #(play2 % (concat rest-players [next-player])) xs)
               (reduce #(merge-with + %1 %2) {})))))))

(defn solve-2
  []
  (let [starts (get-starts! input)
        players [(player 1 (get starts 1)) (player 2 (get starts 2))]]
    (->> (pmap #(play2 % players) xs)
         (reduce #(merge-with + %1 %2) {})
         vals
         (apply max))))
