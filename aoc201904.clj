(defn int->vec-of-ints [s]
  (map #(Integer/parseInt %) (clojure.string/split (.toString s) #"")))

(defn main
  [from to]
  (count (into #{}
               (comp (map int->vec-of-ints)
                     (filter (fn [ps]
                               (and (apply <= ps)
                                    (->> (group-by identity ps)
                                         (vals)
                                         (map count)
                                         ; part 1: >=
                                         ; patt 2: =
                                         (some #(>= % 2)))))))
               (range from (inc to)))))

(comment (main 165432 707912))
; 1716
; 1163
