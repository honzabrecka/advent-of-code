(ns aoc20.day13)

(def input "./data/aoc20/day13.txt")

(defn parse-1
  [data]
  (let [[t ids] (clojure.string/split data #"\n")]
    [(Long/parseLong t)
     (->> (clojure.string/split ids #",")
          (remove #(= % "x"))
          (map #(Long/parseLong %)))]))

(defn solve-1
  []
  (let [[t ids] (parse-1 (slurp input))
        nearest (map #(* (inc (Math/floor (/ t %))) %) ids)
        [[t# id]] (sort (zipmap nearest ids))]
    (* (- t# t) id)))

(defn parse-2
  [data]
  (let [[_ ids] (clojure.string/split data #"\n")]
    (->> (clojure.string/split ids #",")
         (keep-indexed (fn [i x] [x i]))
         (remove (fn [[id]] (= id "x")))
         (map (fn [[id d]] [(Integer/parseInt id) d])))))

(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. "
  [^long a ^long b]
  (cond (zero? a) [(Math/abs b) 0 1]
        (zero? b) [(Math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (Math/abs b)
                     r0 (Math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese-remainder
  " Main routine to return the chinese remainder "
  [n a]
  (let [prod (apply * n)
        _ (println prod)
        reducer (fn [^long sum [n_i a_i]]
                  (let [p (quot prod n_i)           ; p = prod / n_i
                        egcd (extended-gcd p n_i)   ; Extended gcd
                        inv_p (second egcd)]        ; Second item is the inverse
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))] ; Replaces the Python for loop to sum
    ; (map vector n a) is same as
    ;                                             ; Python's version Zip (n, a)
    (mod sum-prod prod)))                             ; Result line

(defn solve-2
  []
  (let [xs (parse-2 (slurp input))
        mods (map (fn [[x n]] (- x n)) xs)]
    (println xs)
    (println mods)
    (chinese-remainder (map first xs) mods)))


(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn reducer
  [[t step] [bus i]]
  (if (zero? (mod (+ t i) bus))
    [t (lcm step bus)]
    (recur [(+ t step) step] [bus i])))

(defn solve-2#
  []
  (let [data (slurp input)
        [_ ids] (clojure.string/split data #"\n")]
    (->> (clojure.string/split ids #",")
         (keep-indexed (fn [i bus] [bus i]))
         (remove (fn [[bus]] (= bus "x")))
         (map (fn [[bus t]] [(Integer/parseInt bus) t]))
         (reduce reducer [0 1])
         first)))
