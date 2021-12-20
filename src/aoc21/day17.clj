(ns aoc21.day17)

(def input-test "target area: x=20..30, y=-10..-5")

(def input "target area: x=70..96, y=-179..-124")

(defn parse-input
  [s]
  (->> (re-matches #"target area: x\=([\-0-9]+)\.\.([\-0-9]+), y\=([\-0-9]+)\.\.([\-0-9]+)" s)
       (drop 1)
       (map parse-long)
       (partition 2)
       (map sort)))

(defn step
  [[[x y] [dx dy]]]
  [[(+ x dx) (+ y dy)]
   [(let [x# (dec dx)]
      (if (= x# -1) 0 x#))
    (dec dy)]])

(defn outside-world?
  [[xs ys] [x y]]
  (or (> x (second xs))
      (< y (first ys))))

(defn inside-target?
  [[xs ys] [x y]]
  (let [[x1 x2] xs
        [y1 y2] ys]
    (and (>= x x1)
         (<= x x2)
         (>= y y1)
         (<= y y2))))

(defn fire
  [target [dx dy]]
  (->> (iterate step [[0 0] [dx dy]])
       (take-while (fn [[[x y] _]]
                     (not (outside-world? target [x y]))))
       (map first)))

(defn solve-1
  []
  (let [target (parse-input input)]
    (->> (for [dx (range 1 200)
               dy (range 1 200)]
           [dx dy])
         (pmap (partial fire target))
         (filter (fn [path] (inside-target? target (last path))))
         (mapcat (fn [path] (map second path)))
         (apply max))))

(defn solve-2
  []
  (let [target (parse-input input)]
    (->> (for [dx (range 1 200)
               dy (range -200 200)]
           [dx dy])
         (pmap (partial fire target))
         (filter (fn [path] (inside-target? target (last path))))
         (count))))
