(ns aoc20.day8
  (:require [clojure.java.io :as io]))

(def input "./data/aoc20/day8.txt")

(defn parse
  [line]
  (let [[_ op sign n] (re-matches #"(nop|jmp|acc) ([+\-])([0-9]+)" line)]
    {:op (keyword op)
     :f  (get {"+" + "-" -} sign)
     :n  (Integer/parseInt n)}))

(defn read-program!
  [input]
  (with-open [reader (io/reader input)]
    (into [] (map parse) (line-seq reader))))

(defn run
  [program]
  (loop [visited-indices #{}
         acc 0
         index 0]
    (if (= index (count program))
      [:ok acc]
      (let [{:keys [op f n]} (get program index)
            visited-indices# (conj visited-indices index)]
        (case op
          :nop (recur visited-indices# acc (inc index))
          :acc (recur visited-indices# (f acc n) (inc index))
          :jmp (let [index# (f index n)]
                 (if (contains? visited-indices# index#)
                   [:error acc]
                   (recur visited-indices# acc index#))))))))

(defn solve-1
  []
  (run (read-program! input)))

(defn solve-2
  []
  (let [program (read-program! input)]
    (into [] (comp (keep-indexed (fn [i x] [x i]))
                   (filter (fn [[{:keys [op]}]] (contains? #{:nop :jmp} op)))
                   (map (fn [[_ index]]
                          (run (update-in program [index :op] {:jmp :nop :nop :jmp}))))
                   (filter (fn [[result]] (= result :ok))))
          program)))
