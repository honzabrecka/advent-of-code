(ns aoc22.day7
  (:require [clojure.java.io :as io]))

(def input "./data/aoc22/day7.txt")

(defn has?
  [m i]
  (not (nil? (nth m i))))

(defn parse-line
  [s]
  (let [m (re-matches #"^(\$ (cd) ([\.a-z\/]+))|(\$ (ls))|(dir ([a-z]+))|(([\d]+) ([\.a-z]+))$" s)]
    (cond
      (has? m 2) [:cd (nth m 3)]
      (has? m 5) [:ls]
      (has? m 9) [:file (nth m 10) (parse-long (nth m 9))]
      (has? m 6) [:dir (nth m 7)]
      :else [:error])))

(defn dir-index
  [ls name]
  (->> ls
       (keep-indexed (fn [i v] [i v]))
       (filter (fn [[_ x]] (= ((juxt :type :name) x) [:dir name])))
       ffirst))

(defn get-tree!
  [input]
  (with-open [reader (io/reader input)]
    (second
      (transduce (map parse-line)
                 (completing
                   (fn reduce [[path acc] [type a b]]
                     (cond
                       (= type :cd)
                       [(cond
                          (= a "/") [:files]
                          (= a "..") (->> path butlast butlast vec)
                          :else (concat path [(dir-index (get-in acc path) a) :files]))
                        acc]
                       (= type :dir)
                       [path (update-in acc path #(conj % {:type :dir :name a :files []}))]
                       (= type :file)
                       [path (update-in acc path #(conj % {:type :file :name a :size b}))]
                       :else
                       [path acc])))
                 [[:files] {:type :dir :name "/" :files []}]
                 (line-seq reader)))))

(defn dir-sizes
  [path {:keys [files]}]
  (->> files
       (reduce (fn [acc {:keys [type name files size]}]
                 (if (= type :dir)
                   (let [path# (conj path name)
                         sizes (dir-sizes path# {:files files})
                         total (->> sizes (filter (fn [[key _]] (= (count key)(count path#))))
                                    vals
                                    (reduce +))]
                     (merge (update acc path #(+ (or % 0) total))
                            sizes))
                   (update acc path #(+ (or % 0) size))))
               {})))

(defn solve-1
  []
  (->> (get-tree! input)
       (dir-sizes ["/"])
       vals
       (filter #(< % 100000))
       (reduce +)))

(defn solve-2
  []
  (let [sizes (->> (get-tree! input)
               (dir-sizes ["/"]))
        total (get sizes ["/"])]
    (->> sizes
         vals
         sort
         (filter #(>= (+ (- 70000000 total) %) 30000000))
         first)))
