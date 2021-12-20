(ns aoc21.day13
  (:require [clojure.java.io :as io]
            [aoc20.shared :as shared]))

(def input "./data/aoc21/day13.txt")

(defn get-input!
  [input]
  (let [[points [_ & folds]]
        (with-open [reader (io/reader input)]
          (->> (line-seq reader)
               (doall)
               (shared/split-by #(= "" %))))]
    [(map #(mapv parse-long (clojure.string/split % #",")) points)
     (map (fn [s]
            (let [[_ pos val] (re-matches #"fold along ([xy])=([0-9]+)" s)]
              [pos (parse-long val)]))
          folds)]))

(def fold-fn
  {"x" first
   "y" second})

(defn solve-1
  []
  (let [[points [fold]] (get-input! input)
        groups (group-by (fn [point]
                           (let [[pos val] fold
                                 x-y ((get fold-fn pos) point)]
                             (< x-y val)))
                         points)]
    (->> (concat (get groups true)
                 (map (fn [point]
                        (let [[pos val] fold]
                          (update point ((get fold-fn pos) [0 1]) #(- val (- % val)))))
                      (get groups false)))
         (set)
         (count))))

(defn print-code
  [points]
  (let [[max-x _] (first (sort-by first > points))
        [_ max-y] (first (sort-by second > points))
        row (vec (repeat (inc max-x) 1))
        m (vec (repeat (inc max-y) row))]
    (reduce (fn [acc [x y]]
              (assoc-in acc [y x] 8))
            m points)))

(defn solve-2
  []
  (let [[points folds] (get-input! input)]
    (loop [points points
           [fold & folds] folds]
      (if (nil? fold)
        (->> points set print-code)
        (let [groups (group-by (fn [point]
                                 (let [[pos val] fold]
                                   (< ((get fold-fn pos) point) val)))
                               points)]
          (recur (concat (get groups true)
                         (map (fn [point]
                                (let [[pos val] fold]
                                  (update point ((get fold-fn pos) [0 1]) #(- val (- % val)))))
                              (get groups false)))
                 folds))))))
