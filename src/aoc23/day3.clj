(ns aoc23.day3
  (:require [clojure.java.io :as io]))

(def input "./data/aoc23/day3.txt")

(def numchars
  #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(def neighbours [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]])

(defn parse-line
  [y line]
  (loop [[c & cs] line
         x 0
         nums []
         specials {}
         open-num nil]
    (let [x' (inc x)
          append-to-nums-if-open (fn [] (if (nil? open-num) nums (conj nums open-num)))]
      (cond
        (nil? c) {:specials specials :nums (append-to-nums-if-open)}
        (= c \.) (recur cs x' (append-to-nums-if-open) specials nil)
        (contains? numchars c) (let [num [[x y ] c]]
                                 (recur cs x' nums specials (if (nil? open-num) [num] (conj open-num num))))
        :else (recur cs x' (append-to-nums-if-open) (assoc specials [x y] c) nil)))))

(defn get-schema!
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (keep-indexed vector)
         (map (fn [[y line]] (parse-line y line)))
         (reduce (fn [acc {:keys [specials nums]}]
                   (-> acc
                       (update :specials merge specials)
                       (update :nums concat nums)))
                 {:specials {} :nums []}))))

(defn nums->index
  [nums]
  (into {} (->> nums
                (keep-indexed vector)
                (mapcat (fn [[index nums']]
                          (map (fn [[point _]] [point index]) nums'))))))

(defn num->long
  [xs]
  (->> xs
       (map second)
       (reduce str "")
       parse-long))

(defn solve-1
  []
  (let [{:keys [specials nums]} (get-schema!)
        index (nums->index nums)]
    (->> specials
         (mapcat (fn [[[x y] _]]
                   (->> neighbours
                        (map (fn [[dx dy]] [(+ dx x) (+ dy y)]))
                        (map #(get index %))
                        (remove nil?))))
         (dedupe)
         (map #(nth nums %))
         (map num->long)
         (reduce +))))

(defn solve-2
  []
  (let [{:keys [specials nums]} (get-schema!)
        index (nums->index nums)]
    (->> specials
         (filter (fn [[_ c]] (= c \*)))
         (map (fn [[[x y] _]]
                (->> neighbours
                     (map (fn [[dx dy]] [(+ dx x) (+ dy y)]))
                     (map #(get index %))
                     (remove nil?)
                     (dedupe))))
         (filter #(= (count %) 2))
         (map (fn [[index-a index-b]]
                (* (num->long (nth nums index-a))
                   (num->long (nth nums index-b)))))
         (reduce +))))

; first step is to read given input line by line
; and parse each line to {:specials :nums} where
;   :specials is a map with point [x y] as key and a char as a value, so no bit of info is lost
;   :nums is a vector (to be able to obtain value by index) of something called "nums"
;     "num" is an ordered vector containing all the positions and numbers

{:specials {[3 1] \*, [6 3] \#, [3 4] \*, [5 5] \+, [3 8] \$, [5 8] \*},
 :nums ([[[0 0] \4] [[1 0] \6] [[2 0] \7]]
        [[[5 0] \1] [[6 0] \1] [[7 0] \4]]
        [[[2 2] \3] [[3 2] \5]]
        ; ...
        [[[5 9] \5] [[6 9] \9] [[7 9] \8]])}

; then index is build from :nums
; a map with point as a key and a numeric index pointing to actual position in :nums as a value

{[8 7] 7,
 [2 2] 2,
 [0 0] 0,
 ; ...
 [3 2] 2}

; then it is simple, go through all the :specials and its neighbours
; look up into index if something's there
; dedupe - because for example this case:
;
;   *
; 123
;
; two identical indices will be mapped, but actually only one num is there

((0 2) (4) (7 9))

; then map matching indices back to :nums
; and convert nums to actual longs
