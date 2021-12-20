(ns aoc21.day16)

(def input "./data/aoc21/day16.txt")

(def hex->bin
  {"0" [0 0 0 0]
   "1" [0 0 0 1]
   "2" [0 0 1 0]
   "3" [0 0 1 1]
   "4" [0 1 0 0]
   "5" [0 1 0 1]
   "6" [0 1 1 0]
   "7" [0 1 1 1]
   "8" [1 0 0 0]
   "9" [1 0 0 1]
   "A" [1 0 1 0]
   "B" [1 0 1 1]
   "C" [1 1 0 0]
   "D" [1 1 0 1]
   "E" [1 1 1 0]
   "F" [1 1 1 1]})

(defn hexs->binv
  [s]
  (mapcat hex->bin (clojure.string/split s #"")))

(defn ->dec
  [col]
  (->> (reverse col)
       (reduce (fn [[acc i] b]
                 [(if (= b 1) (bit-set acc i) acc) (inc i)])
               [0 0])
       first))

(declare parse-packet)

(defn parse-version
  [xs]
  (->dec (take 3 xs)))

(defn parse-id
  [xs]
  (->dec (take 3 (drop 3 xs))))

(defn packet-body
  [xs]
  (drop 6 xs))

(defn x1
  [length xs']
  (loop [xs (take length xs')
         result []]
    (if (> (count xs) 5)
      (let [packet (parse-packet xs)]
        (recur (:rest packet) (conj result (dissoc packet :rest))))
      [(drop length xs') result])))

(defn x2
  [max xs]
  (loop [xs xs
         result []]
    (if (= (count result) max)
      [xs result]
      (let [packet (parse-packet xs)]
        (recur (:rest packet) (conj result (dissoc packet :rest)))))))

(defn parse-operator
  [[length-type-id & xs]]
  (if (zero? length-type-id)
    (let [length (->dec (take 15 xs))]
      (->> (drop 15 xs) (x1 length)))
    (let [length (->dec (take 11 xs))]
      (->> (drop 11 xs) (x2 length)))))

(defn parse-literal
  [xs]
  (loop [rest xs
         result []]
    (let [[x & cur] (take 5 rest)]
      (if (zero? x)
        [(drop 5 rest) (->dec (concat result cur))]
        (recur (drop 5 rest) (concat result cur))))))

(defn parse-body
  [id xs]
  (if (= id 4)
    (parse-literal xs)
    (parse-operator xs)))

(defn parse-packet
  [xs]
  (let [version (parse-version xs)
        id (parse-id xs)
        [rest body] (parse-body id (packet-body xs))]
    {:version version
     :id      id
     :body    body
     :rest    rest}))

(defn r1
  [{:keys [version body]}]
  (if (coll? body)
    (+ version (reduce + (map r1 body)))
    version))

(defn solve-1
  []
  (->> input slurp hexs->binv parse-packet r1))

(defn r2
  [{:keys [id body]}]
  (case id
    0 (->> body (map r2) (reduce +))
    1 (->> body (map r2) (reduce *))
    2 (->> body (map r2) (apply min))
    3 (->> body (map r2) (apply max))
    4 body
    5 (let [[a b] (map r2 body)] (if (> a b) 1 0))
    6 (let [[a b] (map r2 body)] (if (< a b) 1 0))
    7 (let [[a b] (map r2 body)] (if (= a b) 1 0))))

(defn solve-2
  []
  (->> input slurp hexs->binv parse-packet r2))
