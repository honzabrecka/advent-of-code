(ns sandbox.advent.day062019
  (:require [clojure.java.io :as io]))

(defn ->graph
  []
  (with-open [reader (io/reader "./data/aoc2019/day6.txt")]
    (into {}
          (comp (map #(clojure.string/split % #"\)"))
                (map (fn [[parent name]] [name parent])))
          (line-seq reader))))

(defn parents
  [graph name]
  (loop [name name
         path []]
    (let [parent (get graph name)]
      (if (nil? parent)
        path
        (recur parent (conj path parent))))))

(defn main1
  []
  (let [graph (->graph)]
    (->> graph
         (keys)
         (mapcat #(parents graph %))
         (count))))
; 249308

(defn main2
  []
  (let [graph (->graph)
        you (parents graph "YOU")
        san (parents graph "SAN")
        take-until-common (fn [a b]
                            (let [b# (set b)]
                              (take-while #(not (contains? b# %)) a)))]
    (+ (count (take-until-common you san))
       (count (take-until-common san you)))))
; 349
