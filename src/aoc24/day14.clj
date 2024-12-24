(ns aoc24.day14
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day14.txt")

; [x y dx dy]
(defn parse-line
  [line]
  (let [m (re-matches #"p\=([\d]+),([\d]+) v\=(\-?[\d]+),(\-?[\d]+)" line)]
    (->> m
         (drop 1)
         (map parse-long))))

(defn get-input!
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         (into []))))

(defn move-robot
  [w h [x y dx dy]]
  (let [x' (+ x dx)
        y' (+ y dy)]
    [(mod x' w) (mod y' h) dx dy]))

(defn move-robots
  [w h robots]
  (map #(move-robot w h %) robots))

(defn in-quadrant?
  [x y w h [x' y']]
  (and (>= x' x)
       (>= y' y)
       (< x' (+ x w))
       (< y' (+ y h))))

(defn reverse-x
  [w cs]
  (map (fn [[x y]] [(dec (- w (mod (dec x) w))) y]) cs))

; 12
; 34
(defn ->quadrants
  [w h robots]
  (let [w' (int (/ w 2))
        h' (int (/ h 2))
        q (group-by (fn [robot]
                      (cond
                        (in-quadrant? 0 0 w' h' robot) :q1
                        (in-quadrant? (inc w') 0 w' h' robot) :q2
                        (in-quadrant? 0 (inc h') w' h' robot) :q3
                        (in-quadrant? (inc w') (inc h') w' h' robot) :q4
                        :else :no))
                    robots)]
    (-> q
        (update-vals (fn [robots]
                         (map (fn [[x y]] [x y]) robots)))
        (update :q2 #(reverse-x w' %))
        (update :q4 #(reverse-x w' %)))))

(defn result
  [quadrants]
  (->> (dissoc quadrants :no)
       (vals)
       (map count)
       (reduce *)))

(defn solve-1
  []
  (let [w 101
        h 103]
    (->> (nth (iterate #(move-robots w h %) (get-input!)) 100)
         (->quadrants w h)
         (result))))

(defn solve-2
  []
  (let [w 101
        h 103]
    (->> (iterate #(move-robots w h %) (get-input!))
         (keep-indexed vector)
         (some (fn [[i robots]]
                 (when (= (count robots)
                          (count (into #{} (map (fn [[x y]] [x y]) robots))))
                   i)
                 #_(if (= i 10000000)
                   nil
                   (let [{:keys [q1 q2 q3 q4]} (update-vals (->quadrants w h robots) set)]
                     (when (and (= q1 q2)
                                (= q3 q4)
                                )
                       i))))))))

; 01234
; 678910
; -x---
; ---x-


(defn print-grid
  [n]
  (let [w 101
        h 103
        ps (->> (nth (iterate #(move-robots w h %) (get-input!)) n)
                (map (fn [[x y]] [x y]))
                (into #{}))]
    (->> (range h)
         (map (fn [y]
                (->> (range w)
                     (map (fn [x]
                            (if (contains? ps [x y])
                              "X"
                              "-")))
                     (apply str))))
         (clojure.string/join "\n"))))
