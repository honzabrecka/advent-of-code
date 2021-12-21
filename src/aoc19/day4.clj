(ns aoc19.day4)

(defn int->vec-of-ints [s]
  (map #(Integer/parseInt %) (clojure.string/split (.toString s) #"")))

(defn main
  [from to compare]
  (count (into #{}
               (comp (map int->vec-of-ints)
                     (filter (fn [ps]
                               (and (apply <= ps)
                                    (->> (group-by identity ps)
                                         (vals)
                                         (map count)
                                         (some #(compare % 2)))))))
               (range from (inc to)))))
; 1716
; 1163

(defn -main
  []
  (println (main 165432 707912 >=))
  (println (main 165432 707912 =)))
