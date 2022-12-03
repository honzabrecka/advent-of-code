(ns aoc15.day1)

(def input "./data/aoc15/day1.txt")

(defn solve-1
  []
  (let [fqs (frequencies (into [] (slurp input)))]
    (- (get fqs \()
       (get fqs \)))))

(defn solve-2
  []
  (let [ps (into [] (slurp input))]
    (->> (reductions (fn [acc p] ((get {\( inc \) dec} p) acc)) 0 ps)
         (keep-indexed (fn [i x] [x i]))
         (drop-while (fn [[x _]] (>= x 0)))
         first
         second)))
