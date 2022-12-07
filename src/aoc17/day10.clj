(ns aoc17.day10)

(def input "./data/aoc17/day10.txt")

(defn process
  ([list lengths]
   (process list lengths 0 0))
  ([list lengths position skip-size]
   (loop [[l & ls] lengths
          list list
          position position
          skip-size skip-size]
     (if (nil? l)
       [list position skip-size]
       (let [indices (map #(mod (+ position %) (count list)) (range l))
             selection (map list indices)
             list# (reduce (fn [list [index value]]
                             (assoc list index value))
                           list (map vector indices (reverse selection)))]
         (recur ls list# (+ position l skip-size) (inc skip-size)))))))

(defn solve-1
  []
  (let [lengths (->> (clojure.string/split (slurp input) #",")
                     (map parse-long))
        [[a b]] (process (vec (range 256)) lengths)]
    (* a b)))

(defn p
  [s]
  (if (= (count s) 1)
    (str "0" s)
    s))

(defn solve-2
  []
  (let [lengths (concat (map int (slurp input)) [17, 31, 73, 47, 23])
        list (vec (range 256))]
    (->> (nth (iterate (fn [[list position skip-size]]
                         (process list lengths position skip-size))
                       [list 0 0])
              64)
         first
         (partition 16)
         (map #(apply bit-xor %))
         (map #(Integer/toHexString %))
         (map p)
         (reduce str))))
