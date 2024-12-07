(ns aoc24.day3
  (:require [clojure.test :refer :all]))

(def input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def input2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(def input (slurp "./data/aoc24/day3.txt"))

(defn r-prepare-node
  [[p & ps]]
  (cond
    (nil? p) []
    (string? p) [{:node (seq p) :leafs (r-prepare-node ps)}]
    :else (->> (range (:min p) (inc (:max p)))
               reverse
               (map (fn [n] {:node (repeat n (:pattern p)) :grab true :leafs (r-prepare-node ps)})))))

(defn r-prepare
  [& patterns]
  (mapcat r-prepare-node patterns))

(def r-digits (into #{} "0123456789"))

(defn r-single
  [pattern s]
  (loop [[c & cs] s
         [p & ps] pattern
         r ""]
    (cond
      (nil? c) [(if (nil? p) r nil) []]
      (nil? p) [r (conj cs c)]
      (and (= p :d) (contains? r-digits c)) (recur cs ps (str r c))
      (= p c) (recur cs ps (str r c))
      :else [nil (seq s)])))

(deftest test-r-single
  (is (= (r-single "1" "1") ["1" []]))
  (is (= (r-single "1" "123") ["1" [\2 \3]]))
  (is (= (r-single "1" "") [nil []]))
  (is (= (r-single [\1 \2] "12") ["12" []]))
  (is (= (r-single [\1 \2] (seq "12")) ["12" []]))
  (is (= (r-single [:d] "123") ["1" [\2 \3]]))
  (is (= (r-single [:d] "abc") [nil [\a \b \c]]))
  (is (= (r-single [:d :d] "1bc") [nil [\1 \b \c]])))

(defn r-many
  [[{:keys [node grab leafs]} & ps] s res]
  (let [[m s'] (r-single node s)]
    (cond
      (and (nil? m) (nil? ps)) [nil (concat (mapcat seq (map :m res)) (seq s)) []]
      (nil? m) (recur ps s res)
      (or (empty? s') (empty? leafs)) (let [r (conj res {:m m :grab grab})] [(->> r (map :m) (reduce str)) s' (->> r (filter :grab) (map :m))])
      :else (recur leafs s' (conj res {:m m :grab grab})))))

(deftest test-r-many
  (is (= (r-many (r-prepare ["abc"]) "xabc" []) [nil [\x \a \b \c] []]))
  (is (= (r-many (r-prepare ["foo"] ["abc"]) "xabc" []) [nil [\x \a \b \c] []]))
  (is (= (r-many (r-prepare ["abc"]) "abc" []) ["abc" [] []]))
  (is (= (r-many (r-prepare ["foo"] ["abc"]) "abc" []) ["abc" [] []]))
  (is (= (r-many (r-prepare ["foo" "abc"]) "fooab" []) [nil [\f \o \o \a \b] []]))
  (is (= (r-many (r-prepare ["foo" "abc"]) "fooabc" []) ["fooabc" [] []]))
  (is (= (r-many (r-prepare ["foo" "abc"]) "fooabcd" []) ["fooabc" [\d] []]))
  (is (= (r-many (r-prepare ["do"] ["foo" "abc"]) "fooabcd" []) ["fooabc" [\d] []]))
  (is (= (r-many (r-prepare ["foo" "abc"]) "foooabcd" []) [nil (seq "foooabcd") []]))
  (is (= (r-many (r-prepare ["(" {:pattern :d :min 1 :max 2} ")"]) "(1)x" []) ["(1)" [\x] ["1"]]))
  (is (= (r-many (r-prepare ["(" {:pattern :d :min 1 :max 2} ")"]) "(12)x" []) ["(12)" [\x] ["12"]]))
  (is (= (r-many (r-prepare ["(" {:pattern :d :min 1 :max 2} ")"]) "(123)x" []) [nil [\( \1 \2 \3 \) \x] []])))

(defn r-seq
  [patterns s]
  (if (empty? s)
    nil
    (let [[m s' gs] (r-many patterns s [])]
      (cond
        (nil? m) (recur patterns (drop 1 s'))
        :else (lazy-seq (cons [m gs] (r-seq patterns s')))))))

#_(r-seq' (r-prepare ["mul(" {:pattern :d :min 1 :max 2} ")"] ["don't()"] ["do()"]) "xmul(1)xxdo()don't()xmul(2mul(33)xdo()mul(234")


(defn solve-1
  []
  (->> (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" input)
       (reduce (fn [acc [_ a b]]
                 (+ acc
                    (* (parse-long a)
                       (parse-long b))))
               0)))

(defn solve-2
  []
  (->> (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)" input)
       (reduce (fn [[sum enabled?] [m a b]]
                 (cond
                   (and (clojure.string/starts-with? m "mul") enabled?)
                   [(+ sum
                       (* (parse-long a)
                          (parse-long b)))
                    enabled?]
                   (clojure.string/starts-with? m "don't") [sum false]
                   (clojure.string/starts-with? m "do") [sum true]
                   :else [sum enabled?]))
               [0 true])
       first))

(defn solve-2'
  []
  (->> (r-seq (r-prepare ["mul(" {:pattern :d :min 1 :max 3} "," {:pattern :d :min 1 :max 3} ")"] ["don't()"] ["do()"]) input)
       (reduce (fn [[sum enabled?] [m [a b]]]
                 (cond
                   (and (clojure.string/starts-with? m "mul") enabled?)
                   [(+ sum
                       (* (parse-long a)
                          (parse-long b)))
                    enabled?]
                   (clojure.string/starts-with? m "don't") [sum false]
                   (clojure.string/starts-with? m "do") [sum true]
                   :else [sum enabled?]))
               [0 true])
       first))
