(ns aoc20.day17
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def input "./data/aoc20/day17.txt")

(def active \#)

(defn active?
  [x]
  (= x active))

(def inactive \.)

(defn inactive?
  [x]
  (contains? #{inactive nil} x))

(defn parse
  [s]
  (into [] s))

(defn create-grid!
  [dimensions]
  (with-open [reader (io/reader input)]
    (let [initial-grid (into [] (map parse (line-seq reader)))
          width (count (first initial-grid))
          height (count initial-grid)
          grid (into {}
                     (filter (fn [[_ value]] (active? value)))
                     (for [x (range width)
                           y (range height)]
                       [(concat [x y] (repeat (- dimensions 2) 0)) (get-in initial-grid [y x])]))]
      {:grid       grid
       :dimensions dimensions})))

(defn neighbor-positions
  [dimensions position]
  (let [self (repeat dimensions 0)]
    (->> (combo/selections [-1 0 1] dimensions)
         (remove #(= % self))
         (map (fn [plus]
                (mapv + position plus))))))

(defn add-neighbors
  [{:keys [grid dimensions]}]
  {:grid
   (reduce (fn [grid# [position _]]
             (transduce (map (fn [position] [position (get grid position inactive)]))
                        (fn [grid# [position value]] (assoc grid# position value))
                        grid#
                        (neighbor-positions dimensions position))
             #_(->> position
                    (neighbor-positions dimensions)
                    (map (fn [position] [position (get grid position inactive)]))
                    (reduce (fn [grid# [position value]] (assoc grid# position value)) grid#)))
           grid grid)
   :dimensions
   dimensions})

(defn f
  [{:keys [grid dimensions]}]
  (add-neighbors
    {:grid
     (reduce
       (fn [grid# [position value]]
         (let [neighbors (->> position
                              (neighbor-positions dimensions)
                              (map (fn [position] [position (get grid position inactive)])))
               neighbor-fqs (frequencies (map second neighbors))
               value# (cond
                        (and (active? value) (contains? #{2 3} (get neighbor-fqs active))) active
                        (and (inactive? value) (= (get neighbor-fqs active) 3)) active
                        :else inactive)]
           (if (active? value#)
             (assoc grid# position value#)
             grid#)))
       {} grid)
     :dimensions
     dimensions}))

(defn solve-1
  []
  (let [grid (add-neighbors (create-grid! 3))]
    grid
    (->> (iterate f grid)
         (drop 6)
         (first)
         :grid
         (vals)
         (filter active?)
         (count))))

(defn solve-2
  []
  (let [grid (add-neighbors (create-grid! 4))]
    grid
    (->> (iterate f grid)
         (drop 6)
         (first)
         :grid
         (vals)
         (filter active?)
         (count))))
