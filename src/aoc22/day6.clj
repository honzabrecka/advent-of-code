(ns aoc22.day6)

(def input "./data/aoc22/day6.txt")

(defn find-marker
  [n coll]
  (let [[buffer rest] (split-at (dec n) coll)]
    (loop [index n
           [x & xs] rest
           buffer (vec buffer)]
      (let [buffer# (conj buffer x)]
        (cond
          (nil? x) :not-found
          (= (count (set buffer#)) n) index
          :else (recur (inc index) xs (vec (drop 1 buffer#))))))))

(defn find-marker
  [n input]
  (->> input
       (partition n 1)
       (keep-indexed (fn [i v] [i (set v)]))
       (filter (fn [[_ v]] (= (count v) n)))
       ffirst
       (+ n)))

(defn solve-1
  []
  (find-marker 4 (slurp input)))

(defn solve-2
  []
  (find-marker 14 (slurp input)))
