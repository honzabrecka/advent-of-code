(ns aoc20.day16
  (:require [clojure.java.io :as io]))

(def input "./data/aoc20/day16.txt")

(defn ->int
  [s]
  (Integer/parseInt s))

(def split-by-empty-line
  (comp (partition-by #{""})
        (remove #(= % [""]))))

(defn parse-field
  [row]
  (let [[_ field a b c d] (re-matches #"(.+?): ([\d]+)\-([\d]+) or ([\d]+)\-([\d]+)" row)]
    [field
     [(->int a) (inc (->int b))]
     [(->int c) (inc (->int d))]]))

(defn parse-ticket
  [row]
  (->> (clojure.string/split row #",")
       (map ->int)))

(defn get-input!
  []
  (with-open [reader (io/reader input)]
    (let [[fields [_ ticket] [_ & nearby]] (into [] split-by-empty-line (line-seq reader))]
      {:fields (map parse-field fields)
       :ticket (parse-ticket ticket)
       :nearby (map parse-ticket nearby)})))

(defn in-any-field?
  [fields]
  (let [used (->> fields
                  (mapcat (fn [[_ [a b] [c d]]]
                            [(set (range a b))
                             (set (range c d))]))
                  (reduce clojure.set/union))]
    (fn [n]
      (contains? used n))))

(defn solve-1
  []
  (let [{:keys [fields nearby]} (get-input!)]
    (->> nearby
         (mapcat identity)
         (remove (in-any-field? fields))
         (reduce +))))

(defn field-by-n
  [fields]
  (reduce
    (fn [acc [field [a b] [c d]]]
      (reduce
        (fn [acc n]
          (update acc n #(if (nil? %) #{field} (conj % field))))
        acc (concat (range a b) (range c d))))
    {} fields))

(defn determine-order
  [intersections]
  (loop [xs (map (fn [s] [s nil]) intersections)]
    (if (->> xs (map second) (some nil?))
      (let [[x] (first (filter (fn [[s _]] (= (count s) 1)) xs))
            w# (first x)]
        (recur (map (fn [[s w]] [(disj s w#) (if (= s x) w# w)]) xs)))
      (map second xs))))

(defn solve-2
  []
  (let [{:keys [fields nearby ticket]} (get-input!)
        in-any-field? (in-any-field? fields)
        valid-nearby (filter #(every? in-any-field? %) nearby)
        index (field-by-n fields)]
    (->> (apply map vector valid-nearby)
         (map (fn [indices]
                (map #(get index %) indices)))
         (map #(apply clojure.set/intersection %))
         determine-order
         (map vector ticket)
         (filter (fn [[_ field]]
                   (clojure.string/starts-with? field "departure")))
         (map first)
         (reduce *))))
