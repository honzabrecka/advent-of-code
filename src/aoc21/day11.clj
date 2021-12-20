(ns aoc21.day11
  (:require [clojure.java.io :as io]))

(def input "./data/aoc21/day11.txt")

(defn parse-line
  [s]
  (mapv parse-long (clojure.string/split s #"")))

(defn get-input!
  [input]
  (with-open [reader (io/reader input)]
    (into []
          (map parse-line)
          (line-seq reader))))

(def size 10)

(defn adjacent
  [m [x y]]
  (->> (for [dx (range -1 2)
             dy (range -1 2)
             :when (not (and (zero? dx)
                             (zero? dy)))]
         [(+ x dx) (+ y dy)])
       (remove (fn [[x y]]
                 (or (nil? (get-in m [y x]))
                     (> (get-in m [y x]) 9))))
       (concat [[x y]])))

(defn step#
  [m [x y]]
  (if (= (get-in m [y x]) 10)
    (let [ads (adjacent m [x y])
          fns (comp #(reduce step# % ads)
                    #(reduce (fn [m [x y]]
                               (update-in m [y x] inc))
                             % ads))]
      (fns m))
    m))

(defn step
  [m]
  (let [ps (for [x (range size)
                 y (range size)]
             [x y])
        fns (comp (fn [m]
                    (reduce (fn [m [x y]]
                              (update-in m [y x] #(if (> % 9) 0 %)))
                            m ps))
                  #(reduce step# % ps)
                  (fn [m]
                    (mapv #(mapv inc %) m)))]
    (fns m)))

(defn highlights-count
  [m]
  (->> (apply concat m)
       (filter zero?)
       (count)))

(defn solve-1
  []
  (->> (get-input! input)
       (iterate step)
       (take (inc 100))
       (map highlights-count)
       (reduce +)))

(defn solve-2
  []
  (->> (get-input! input)
       (iterate step)
       (keep-indexed vector)
       (drop-while (fn [[_ m]]
                     (not-every? zero? (apply concat m))))
       (ffirst)))
