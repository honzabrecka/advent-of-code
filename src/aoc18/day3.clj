(ns aoc18.day3
  (:require [clojure.java.io :as io]
            [sandbox.advent.day2 :as day2]
            [clojure.test :refer [deftest is testing run-tests]]))

(def input "./data/aoc18/day3.txt")

(defn parse-claim
  [square]
  (let [[_ id x y w h]
        (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" square)]
    {:id id
     :x  (Integer/parseInt x)
     :y  (Integer/parseInt y)
     :w  (Integer/parseInt w)
     :h  (Integer/parseInt h)}))

(deftest parse-claim-test
  (is (= (parse-claim "#1256 @ 569,326: 23x18")
         {:id "1256" :x 569 :y 326 :w 23 :h 18})))

(defn claim->rectangle
  [{:keys [id x y w h]}]
  (with-meta (for [x# (range w)
                   y# (range h)]
               [(+ x x#) (+ y y#)])
             {:id id}))

(deftest claim->rectangle-test
  (let [claim {:id "1256" :x 2 :y 3 :w 2 :h 1}]
    (is (= (claim->rectangle claim)
           [[2 3] [3 3]]))
    (is (= (meta (claim->rectangle claim))
           {:id "1256"}))))

(defn claims
  [input]
  (with-open [reader (io/reader input)]
    (into []
          (comp (map parse-claim)
                (map claim->rectangle))
          (line-seq reader))))

(defn filter-inches-by-hits
  [compare hits claims]
  (->> (apply concat claims)
       (frequencies)
       (filter (fn [[_ h]] (compare h hits)))))

(deftest filter-inches-by-hits-test
  (is (= (filter-inches-by-hits = 2 [[[2 3] [3 3]]
                                     [[1 3] [2 3]]])
         [[[2 3] 2]])))

(defn solve-1
  []
  (->> (claims input)
       (filter-inches-by-hits > 1)
       (count)))

(defn solve-2
  []
  (let [claims (claims input)
        inches-with-single-hit (into {} (filter-inches-by-hits = 1 claims))]
    (->> claims
         (day2/find (fn [claim]
                      (every? #(get inches-with-single-hit %) claim)))
         (meta))))
