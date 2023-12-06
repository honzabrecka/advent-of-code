(ns aoc23.day5
  (:require [clojure.java.io :as io]))

(def input "./data/aoc23/day5.txt")

(defn section-seq
  ([line-parser]
   (section-seq line-parser conj (conj)))
  ([line-parser reducer]
   (section-seq line-parser reducer (reducer)))
  ([line-parser reducer init]
   (fn [xf]
     (let [state (volatile! init)
           last-input (volatile! nil)]
       (fn
         ([] (xf))
         ([result]
          (xf (xf result @state)))
         ([result input]
          (let [last-input# @last-input]
            (vreset! last-input input)
            (if (= input "")
              (if (= last-input# "")
                result
                (let [v @state]
                  (vreset! state init)
                  (xf result v)))
              (do (vswap! state reducer (line-parser input))
                  result)))))))))

(defn get-input!
  []
  (with-open [reader (io/reader input)]
    (into [] (section-seq identity) (line-seq reader))))

(defn parse-map
  [[id & maps]]
  {:id     (first (clojure.string/split id #" "))
   :ranges (->> maps
                (map (fn [m] (map parse-long (clojure.string/split m #" ")))))})

(defn get-almanac!
  []
  (let [[[seeds-raw] & maps-raw] (get-input!)
        [_ seeds] (clojure.string/split seeds-raw #": ")]
    {:seeds (map parse-long (clojure.string/split seeds #" "))
     :maps  (map parse-map maps-raw)}))

(defn get-mapped-value
  [ranges seed]
  (if-let [found (some (fn [[destination-start source-start length]]
                         (when (and (>= seed source-start)
                                    (< seed (+ source-start length)))
                           (let [d (- seed source-start)]
                             (+ destination-start d))))
                       ranges)]
    found
    seed))

(defn solve-1
  []
  (let [{:keys [seeds maps]} (get-almanac!)]
    (->> seeds
         (map (fn [seed]
                (reduce (fn [seed' {:keys [ranges]}]
                          (get-mapped-value ranges seed'))
                        seed maps)))
         (apply min))))

(defn invalid-interval
  [[_ [a b]]]
  (= a b))

(defn divide-interval
  [interval [x & xs]]
  (if (nil? x)
    [interval]
    (let [[d [from to]] interval
          [destination-start source-start length] x
          a source-start
          b (+ source-start length)
          d' (+ d (- destination-start source-start))
          rec (fn [intervals]
                (->> intervals
                     (mapcat #(divide-interval % xs))
                     (remove invalid-interval)))]
      (cond
        ; [[  ]] [  ]
        (>= a to) (concat [] (divide-interval interval xs))
        ; [  ] [[  ]]
        (< b from) (concat [] (divide-interval interval xs))
        ; [   [[  ]]  ]
        (and (< a from) (> b to)) (divide-interval [d' [from to]] xs)
        ; [[  [  ] ]]
        (and (>= a from) (<= b to)) (rec [[d [from a]] [d' [a b]] [d [b to]]])
        ; [ [[ ]  ]]
        (and (< a from) (<= b to)) (rec [[d' [from b]] [d [b to]]])
        ; [[ [  ]]  ]
        (and (>= a from) (>= b to)) (rec [[d [from a]] [d' [a to]]])))))

(defn apply-shift
  [intervals]
  (map (fn [[d [from to]]] [(+ d from) (+ d to)]) intervals))

(defn process
  [intervals maps]
  (reduce (fn [intervals' {:keys [ranges]}]
            (mapcat #(-> (divide-interval [0 %] ranges) apply-shift) intervals'))
          intervals
          maps))

(defn solve-2
  []
  (let [{:keys [seeds maps]} (get-almanac!)]
    (->> #_(map (fn [seed] [seed 1]) seeds)
         (partition 2 seeds)
         (mapcat (fn [[start length]]
                   (process [[start (+ start length)]] maps)))
         (map first)
         (apply min))))
