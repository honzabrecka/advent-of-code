(ns aoc24.day19
  (:require [clojure.java.io :as io]))

(def input "./data/aoc24/day19.txt")

(defn get-input!
  []
  (let [[towels _ & designs] (with-open [reader (io/reader input)]
                               (doall (line-seq reader)))]
    {:towels  (clojure.string/split towels #", ")
     :designs designs}))

(defn pos-1
  ([towels design]
   (pos-1 towels design []))
  ([towels design result]
   (if (empty? design)
     result
     (->> towels
          (filter #(clojure.string/starts-with? design %))
          (some #(pos-1 towels (clojure.string/replace-first design % "") (conj result %)))))))

(def pos-2
  (memoize
    (fn [towels design]
      (if (empty? design)
        1
        (->> towels
             (filter #(clojure.string/starts-with? design %))
             (map #(pos-2 towels (clojure.string/replace-first design % "")))
             (reduce +))))))

(defn solve-1
  []
  (let [{:keys [towels designs]} (get-input!)]
    (->> designs
         (map #(pos-1 towels %))
         (remove empty?)
         (count))))

(defn solve-2
  []
  (let [{:keys [towels designs]} (get-input!)]
    (->> designs
         (map #(pos-2 towels %))
         (reduce +))))
