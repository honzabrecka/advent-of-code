(ns aoc17.day9)

(def input "./data/aoc17/day9.txt")

(defn parse
  ([s]
   (first (parse s false)))
  ([s garbage?]
   (loop [[x & xs] s
          result []
          garbage? garbage?]
     (if garbage?
       (cond
         (= x \!) (recur (drop 1 xs) result true)
         (= x \>) [result xs]
         :else (recur xs (conj result x) true))
       (cond
         (nil? x) result
         (= x \{) (let [[result# xs#] (parse xs false)]
                    (recur xs# (conj result {:type :group :children result#}) false))
         (= x \}) [result xs]
         (= x \<) (let [[result# xs#] (parse xs true)]
                    (recur xs# (conj result {:type :garbage :children result#}) false))
         (= x \,) (recur xs result false)
         :else {:type :error :children [x]})))))

(defn score
  ([tree] (score 1 tree))
  ([x {:keys [type children]}]
   (if (= type :group)
     (reduce + x (map #(score (inc x) %) children))
     0)))

(defn solve-1
  []
  (score (parse (slurp input))))

(defn count-garbage
  [{:keys [type children]}]
  (cond
    (= type :group) (reduce + (map count-garbage children))
    (= type :garbage) (count children)
    :else 0))

(defn solve-2
  []
  (count-garbage (parse (slurp input))))
