(ns aoc23.day6)

(def input ["35     93     73     66"
            "212   2060   1201   1044"])

(defn input-1
  []
  (->> input
       (map #(clojure.string/split % #"[ ]+"))
       (map #(map parse-long %))
       (apply map vector)))

(defn beats-record?
  [duration distance i]
  (> (* (- duration i) i) distance))

(defn count-of-ways-to-beat-record
  [duration distance]
  (loop [i (int (/ duration 2))
         d i]
    (let [r? (beats-record? duration distance i)
          d' (int (/ d 2))]
      (cond
        (= d 1) (inc (- duration (* 2 (if r? i (inc i)))))
        r? (recur (- i d') d')
        :else (recur (+ i d') d')))))

(defn solve-1
  []
  (->> (input-1)
       (map #(apply count-of-ways-to-beat-record %))
       #_(map (fn [[duration distance]]
              (->> (range (inc duration))
                   (map #(beats-record? duration distance %))
                   (filter true?)
                   (count))))
       (reduce *)))

(defn input-2
  []
  (map #(parse-long (clojure.string/replace % #"[ ]+" "")) input))

(defn solve-2
  []
  (apply count-of-ways-to-beat-record (input-2)))
