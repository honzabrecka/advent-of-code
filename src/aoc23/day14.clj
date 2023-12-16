(ns aoc23.day14
  (:require [clojure.java.io :as io]))

(def input "./data/aoc23/day14.txt")

(defn get-grid!
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map vec)
         (into []))))

(defn dot?
  [c]
  (= c \.))

(defn rock?
  [c]
  (= c \O))

(defn stop?
  [c]
  (= c \#))

(defn append-dots
  [shift result]
  (into result (repeat shift \.)))

(defn slide-column
  [grid x]
  (loop [y 0
         result []
         shift 0]
    (let [v (get-in grid [y x])]
      (cond
        (nil? v) (append-dots shift result)
        (dot? v) (recur (inc y) result (inc shift))
        (stop? v) (recur (inc y) (conj (append-dots shift result) v) 0)
        :else (recur (inc y) (conj result v) shift)))))

(defn rotate
  [matrix]
  (apply map vector matrix))

(def mutators
  {:n [#(->> %) #(->> % rotate vec)]
   :s [#(->> % reverse vec) #(->> % rotate vec reverse vec)]
   :w [#(->> % rotate vec) #(->> %)]
   :e [#(->> % rotate vec reverse vec) #(->> % rotate vec reverse vec rotate vec)]})

(defn slide
  [d grid]
  (let [[before after] (get mutators d)
        grid' (before grid)
        w (count (get grid' 0))]
    (->> (range w)
         (mapv #(slide-column grid' %))
         after)))

(defn score
  [grid]
  (->> grid
       reverse
       (keep-indexed vector)
       (map (fn [[i xs]]
              (* (inc i) (count (filter rock? xs)))))
       (reduce +)))

(defn solve-1
  []
  (->> (get-grid!)
       (slide :n)
       score))

(defn cycle
  [grid]
  (->> grid
       (slide :n)
       (slide :w)
       (slide :s)
       (slide :e)))

(defn invert
  [m]
  (into {} (map (fn [[k v]] [v k]) m)))

(defn solve-2
  []
  (let [grid (get-grid!)
        time 1000000000]
    (loop [[x & xs] (iterate cycle grid)
           i 0
           seen {}]
      (if-let [start (get seen x)]
        (let [period (- i start)
              i' (+ start (mod (- time start) period))]
          (score (get (invert seen) i')))
        (recur xs (inc i) (assoc seen x i))))))
