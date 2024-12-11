(ns aoc24.day11)

(def input [475449 2599064 213 0 2 65 5755 51149])

(defn split-stone
  [stone]
  (let [seq->long (fn [cs] (parse-long (reduce str "" cs)))
        [a b] (split-at (/ (count stone) 2) stone)]
    [(seq->long a) (seq->long b)]))

(defn next-stones
  [stone]
  (let [stone' (str stone)]
    (cond
      (= stone 0) [1]
      (even? (count stone')) (let [[a b] (split-stone stone')] [a b])
      :else [(* stone 2024)])))

(defn count-stones
  [stones]
  (->> (vals stones)
       (reduce +)))

(defn blink-seq'
  [stones]
  (lazy-seq
    (cons (count-stones stones)
          (blink-seq'
            (->> stones
                 (mapcat
                   (fn [[stone count]]
                     (->> (next-stones stone)
                          (map (fn [stone] {stone count})))))
                 (apply merge-with +))))))

(defn blink-seq
  [stones]
  (blink-seq' (frequencies stones)))

(defn solve-1
  []
  (nth (blink-seq input) 25))

(defn solve-2
  []
  (nth (blink-seq input) 75))

(def count'
  (memoize
    (fn
      ([stone max-level]
       (count' stone 0 max-level))
      ([stone level max-level]
       (let [stone' (str stone)]
         (cond
           (= level max-level) 1
           (= stone 0) (count' 1 (inc level) max-level)
           (even? (count stone')) (let [[a b] (split-stone stone')]
                                    (+ (count' a (inc level) max-level)
                                       (count' b (inc level) max-level)))
           :else (count' (* stone 2024) (inc level) max-level)))))))

(defn solve-2'
  []
  (transduce (map #(count' % 0 75)) + input))

; (time (solve-2))
; "Elapsed time: 175.81225 msecs"
; 228449040027793

; (time (solve-2'))
; "Elapsed time: 145.286167 msecs"
; 228449040027793
