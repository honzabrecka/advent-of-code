(ns aoc17.day7
  (:require [clojure.java.io :as io]))

(def input "./data/aoc17/day7.txt")

(defn parse-line
  [s]
  (let [[_ name weight _ children] (re-matches #"^([a-z]+) \(([\d]+)\)( -> ([ a-z,]+))?$" s)]
    {:name     name
     :weight   (parse-long weight)
     :children (if (nil? children)
                 []
                 (clojure.string/split children #", "))}))

(defn find-root
  [xs]
  (let [parents (mapcat (fn [{:keys [name children]}]
                          (map (fn [child] [child name]) children)) xs)
        m (into {} parents)]
    (loop [; no orphans, so each child leads to root - take first
           parent (ffirst parents)]
      (let [parent# (get m parent)]
        (if (nil? parent#)
          parent
          (recur parent#))))))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         find-root)))

(defn find
  [pred coll]
  (first (filter pred coll)))

(defn error?
  [x]
  (= (first x) :error))

(def error-value second)

(defn weight-branch
  [m name]
  (let [{:keys [children weight]} (get m name)]
    (if (empty? children)
      [weight 0]
      (let [ws (map #(weight-branch m %) children)
            error (find error? ws)]
        (if error
          [:error (error-value error)]
          (if (apply = (map first ws))
            ; [total weight of children, weight of this node]
            [(reduce + weight (map first ws)) weight]
            [:error ws]))))))

(defn get-correct-weight
  [xs]
  (let [m (into {} (map (fn [x] [(:name x) x])) xs)
        root (find-root xs)
        w (error-value (weight-branch m root))
        [[max v] [min]] (sort-by first > w)]
    (- v (- max min))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-line)
         get-correct-weight)))
