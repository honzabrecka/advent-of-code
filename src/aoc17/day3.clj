(ns aoc17.day3)

; up -> left -> down -> right
; 3
; [1, right, [1]]
; [2, up, [2, 3]]
; ...

(def input 361527)

(defn up [x y] [x (inc y)])

(defn down [x y] [x (dec y)])

(defn right [x y] [(inc x) y])

(defn left [x y] [(dec x) y])

(defn grid
  ([]
   (cons [0 0]
         (grid [1 0] 3 1 1 1)))
  ([[x y] d s r r2]
   (let [end-direction? (= r s)
         end-step? (= r2 (* 2 s))
         next-dir (fn [] (mod (inc d) 4))]
     (lazy-seq
       (cons [x y]
             (grid (case (if end-direction? (next-dir) d)
                     0 (up x y)
                     1 (left x y)
                     2 (down x y)
                     3 (right x y))
                   (if end-direction? (next-dir) d)
                   (if end-step? (inc s) s)
                   (if end-direction? 1 (inc r))
                   (if end-step? 1 (inc r2))))))))

(defn solve-1
  []
  (->> (nth (grid) (dec input))
       (map abs)
       (reduce +)))

(defn neighbours
  [x y]
  (for [dx [-1 0 1]
        dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ dx x) (+ dy y)]))

(defn solve-2
  []
  (let [grid# (take input (grid))]
    (->> (drop 1 grid#)
         (reduce (fn [grid p]
                   (let [[i _] (get grid p)
                         ns (set (apply neighbours p))
                         score (->> (clojure.set/intersection ns (into #{} (take i grid#)))
                                    (map grid)
                                    (map second)
                                    (reduce +))]
                    (if (> score input)
                      (reduced score)
                      (assoc grid p [i score]))))
                 ; { [x y] [index score], ... }
                 ; index is used to get previous squares (from realized (grid))
                 ; then each (grid) square gets its neighbours
                 ; then score is computed from intersected neighbours with previous squares
                 (assoc
                   (into {} (keep-indexed (fn [i p] [p [i 0]]) grid#))
                   [0 0]
                   [0 1])))))
