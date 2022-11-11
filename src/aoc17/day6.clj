(ns aoc17.day6)

(def input [5	1	10	0	1	7	13	14	3	12	8	10	7	12	0	6])

(defn find-max
  "returns [index value]"
  [ns]
  (->> ns
       (keep-indexed vector)
       (sort-by second >)
       (first)))

(defn next-index
  [ns i]
  (let [l (count ns)]
   (mod (+ l (inc i)) l)))

(defn redistribute
  [ns i]
  (let [l (count ns)]
    (loop [ns' (assoc ns i 0)
           i' (next-index ns i)
           x (get ns i)]
      (if (zero? x)
        ns'
        (recur (update ns' i' inc)
               (next-index ns i')
               (dec x))))))

(defn solve-1
  []
  (loop [ns input
         seen #{ns}]
    (let [[index _] (find-max ns)
          result (redistribute ns index)]
      (if (contains? seen result)
        (count seen)
        (recur result (conj seen result))))))

(defn solve-2
  []
  (loop [ns input
         seen #{ns}
         cycles [ns]]
    (let [[index _] (find-max ns)
          result (redistribute ns index)]
      (if (contains? seen result)
        (- (count seen)
           (->> cycles
                (keep-indexed vector)
                (filter #(= result (second %)))
                (first)
                (first)))
        (recur result (conj seen result) (conj cycles result))))))
