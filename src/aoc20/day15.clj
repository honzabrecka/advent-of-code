(ns aoc20.day15)

(def input (->> (clojure.string/split "0,13,16,17,1,10,6" #",")
                (map #(Integer/parseInt %))))

(defn add-seen
  [seen index n]
  (update seen n
          (fn [[last-index]]
            (if (nil? last-index)
              (list index)
              ; this is the line - instead of (cons index last-index)
              [index last-index]))))

(defn f
  [{:keys [index seen n input]}]
  (let [index# (inc index)
        [x & xs] input]
    (if (nil? x)
      (let [[a b] (get seen n)
            n# (if (nil? b) 0 (- a b))]
        {:index index# :seen (add-seen seen index# n#) :n n# :input []})
      {:index index# :seen (add-seen seen index# x) :n x :input xs})))

(defn init
  [input]
  (let [[x & xs] input
        initial-index 1]
    {:index initial-index
     :seen  (hash-map x (list initial-index))
     :n     x
     :input xs}))

(defn solve-1
  []
  (->> (iterate f (init input))
       (drop (dec 2020))
       (first)
       :n))

(defn solve-2
  []
  (->> (iterate f (init input))
       (drop (dec 30000000))
       (first)
       :n))
