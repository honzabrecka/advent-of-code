(ns aoc21.day7)

(def input "./data/aoc21/day7.txt")

(defn parse-input
  [s]
  (map parse-long (clojure.string/split s #",")))

(defn compute
  [col x]
  (->> col
       (map #(Math/abs (- % x)))
       (reduce +)))

(defn factorial
  [n]
  (reduce + (range 1 (inc n))))

(defn compute2
  [col x]
  (->> col
       (map #(factorial (Math/abs (- % x))))
       (reduce +)))

(defn solve-1
  []
  (let [ns (parse-input (slurp input))
        m (apply max ns)]
    (->> (range 0 m)
         (pmap (fn [x] [x (compute ns x)]))
         (sort-by second)
         (first))))

(defn solve-2
  []
  (let [ns (parse-input (slurp input))
        m (apply max ns)]
    (->> (range 0 m)
         (pmap (fn [x] [x (compute2 ns x)]))
         (sort-by second)
         (first))))
