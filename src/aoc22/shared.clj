(ns aoc22.shared)

(defn section-seq
  ([line-parser]
   (section-seq line-parser conj (conj)))
  ([line-parser reducer]
   (section-seq line-parser reducer (reducer)))
  ([line-parser reducer init]
   (fn [xf]
     (let [state (volatile! init)
           last-input (volatile! nil)]
       (fn
         ([] (xf))
         ([result]
          (xf (xf result @state)))
         ([result input]
          (let [last-input# @last-input]
            (vreset! last-input input)
            (if (= input "")
              (if (= last-input# "")
                result
                (let [v @state]
                  (vreset! state init)
                  (xf result v)))
              (do (vswap! state reducer (line-parser input))
                  result)))))))))
