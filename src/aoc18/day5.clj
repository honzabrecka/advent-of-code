(ns aoc18.day5
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is testing run-tests]]))

(def input "./data/aoc18/day5.txt")

(defn same-type?
  [a b]
  (= (s/upper-case a)
     (s/upper-case b)))

(deftest same-type?-test
  (is (true? (same-type? "a" "A")))
  (is (true? (same-type? "A" "a")))
  (is (true? (same-type? "A" "A")))
  (is (true? (same-type? "a" "a")))
  (is (false? (same-type? "a" "b"))))

(defn opposite-polarity?
  [a b]
  (not= a b))

(deftest opposite-polarity?-test
  (is (true? (opposite-polarity? "a" "A")))
  (is (false? (opposite-polarity? "a" "a")))
  (is (false? (opposite-polarity? "A" "A"))))

(defn react?
  [a b]
  (and (same-type? a b)
       (opposite-polarity? a b)))

(deftest react?-test
  (is (true? (react? "a" "A")))
  (is (false? (react? "a" "a")))
  (is (false? (react? "a" "b"))))

(defn react
  [input]
  (loop [result []
         input' (seq input)]
    (let [[x & xs] input']
      (if xs
        (if (react? x (first xs))
          (recur (vec (butlast result))
                 (if (last result)
                   (concat [(last result)] (next xs))
                   (next xs)))
          (recur (conj result x) xs))
        (s/join (conj result x))))))

(defn react-2
  [result input]
  (let [[x & xs] input]
    (if xs
      (if (react? x (first xs))
        (fn [] (react-2 (vec (butlast result))
                        (if (last result)
                          (concat [(last result)] (next xs))
                          (next xs))))
        (fn [] (react-2 (conj result x) xs)))
      (s/join (conj result x)))))

(deftest react-test
  (is (= (react "dabAcCaCBAcCcaDA")
         "dabCBAcaDA")))

(defn solve-1
  [input]
  (->> (re-seq #".{1,500}" input)
       (pmap react)
       (s/join)
       (react)
       (count)))

(deftest solve-1-test
  (is (= (solve-1 "dabAcCaCBAcCcaDA")
         (count "dabCBAcaDA"))))

(defn remove-x
  [input x]
  (->> (seq input)
       (filter #(not= (s/lower-case %) x))
       (s/join)))

(deftest remove-x-test
  (is (= (remove-x "abcDcCabEC" "c")
         "abDabE")))

(defn solve-2
  [input]
  (let [abc (into #{} (map s/lower-case) input)]
    (->> abc
         (pmap #(->> % (remove-x input) solve-1))
         (apply min))))

(deftest solve-2-test
  (is (= (solve-2 "dabAcCaCBAcCcaDA")
         4)))

(comment
  (time (->> input slurp solve-1))
  (time (->> input slurp solve-2)))
