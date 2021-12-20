(ns aoc21.day6)

(def input "./data/aoc21/day6.txt")

(defn parse-input
  [s]
  (map parse-long (clojure.string/split s #",")))

(defn day
  [xs]
  (mapcat (fn [x]
            (if (zero? x)
              [6 8]
              [(dec x)]))
          xs))

(defn solve-1
  []
  (->> (iterate day (parse-input (slurp input)))
       (take (inc 80))
       (last)
       (count)))

(defn parse-input2
  [s]
  (merge (into {} (map (fn [x] [x 0])) (range 0 (inc 8)))
         (frequencies (map parse-long (clojure.string/split s #",")))))

(defn day2
  [xs]
  (let [m (update-keys xs dec)
        x (get m -1)]
    (if (nil? x)
      m
      (-> m
          (dissoc -1)
          (update 6 #(+ % x))
          (assoc 8 x)))))

(defn solve-2
  []
  (->> (iterate day2 (parse-input2 (slurp input)))
       (take (inc 256))
       (last)
       (vals)
       (reduce +)))
