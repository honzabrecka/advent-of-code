(ns aoc21.day20
  (:require [clojure.java.io :as io]))

(def input "./data/aoc21/day20.txt")

(def ->bin
  {"#" 1
   "." 0})

(defn ->dec
  [col]
  (->> (reverse col)
       (reduce (fn [[acc i] b]
                 [(if (= b 1) (bit-set acc i) acc) (inc i)])
               [0 0])
       first))

(defn parse-line
  [s]
  (mapv ->bin (clojure.string/split s #"")))

(defn image->map
  [image]
  (let [w (count (first image))
        h (count image)]
    (into {}
          (map (fn [[x y]] [[x y] (get-in image [y x])]))
          (for [x (range w)
                y (range h)]
            [x y]))))

(defn get-input!
  [input]
  (let [[enhance _ & image]
        (with-open [reader (io/reader input)]
          (into []
                (map parse-line)
                (line-seq reader)))]
    {:image   (image->map (vec image))
     :enhance enhance}))

(defn neighbours
  [image [x y] default]
  (into []
        (map (fn [[x y]] (get image [x y] default)))
        (for [dy (range -1 2)
              dx (range -1 2)]
          [(+ x dx) (+ y dy)])))

(defn minimal-image-size
  [image]
  (let [d 2
        xs (->> image keys (map first) sort)
        ys (->> image keys (map second) sort)]
    [[(- (first xs) d) (+ (last xs) d)]
     [(- (first ys) d) (+ (last ys) d)]]))

(defn enhance-image
  [enhance image max]
  (loop [image image
         i 1]
    (if (= i (inc max))
      (->> image vals (remove zero?) count)
      (recur (let [[[x1 x2] [y1 y2]] (minimal-image-size image)
                   ; it's infinite image and it's blinking from 0 to 1 -> need to grab even iterations
                   default (if (zero? (mod i 2)) (->bin "#") (->bin "."))]
               (->> (for [x (range x1 (inc x2))
                          y (range y1 (inc y2))]
                      [[x y] (->> (neighbours image [x y] default) ->dec (get enhance))])
                    (reduce (fn [acc [[x y] v]]
                              (assoc acc [x y] v))
                            {})))
             (inc i)))))

(defn solve-1
  []
  (let [{:keys [image enhance]} (get-input! input)]
    (enhance-image enhance image 2)))

(defn solve-2
  []
  (let [{:keys [image enhance]} (get-input! input)]
    (enhance-image enhance image 50)))
