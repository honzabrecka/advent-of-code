(ns aoc18.day4
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing run-tests]]))

(def input "./data/aoc18/day4.txt")

(defn max-by
  [f col]
  (last (sort-by f col)))

(deftest max-by-test
  (is (= (max-by identity [3 1 2]) 3)))

(defn parse-record
  [record]
  (let [[_ year month day hour minute action]
        (re-matches #"^\[(\d+)\-(\d+)\-(\d+) (\d+):(\d+)\] (.+?)$" record)]
    {:datetime [year month day hour minute]
     :action   action}))

(deftest parse-record-test
  (is (= (parse-record "[1518-11-01 23:58] Guard #99 begins shift")
         {:datetime ["1518" "11" "01" "23" "58"]
          :action   "Guard #99 begins shift"})))

(defn datetime->str
  [{:keys [datetime]}]
  (reduce str datetime))

(defn partition-by-shift
  [col]
  (reduce (fn [col record]
            (if (clojure.string/starts-with? (:action record) "Guard")
              (conj col [record])
              (update col (dec (count col)) #(conj % record))))
          [] col))

(defn sleeps
  [[x & xs]]
  (let [[_ guard-id]
        (re-matches #"Guard #(\d+) begins shift" (:action x))]
    {:guard-id (Integer/parseInt guard-id)
     :sleeps   (->> xs
                    (map (fn [{:keys [datetime]}]
                           (Integer/parseInt (get datetime 4))))
                    (partition 2))}))

(defn merge-grouped-records
  [records]
  (merge (first records)
         {:sleeps (apply concat (map :sleeps records))}))

(defn total-sleep-length
  [{:keys [sleeps]}]
  (->> sleeps
       (map (fn [[start end]] (- end start)))
       (reduce +)))

(defn most-sleeping-minute
  [{:keys [guard-id sleeps]}]
  (let [[minute frequency]
        (->> sleeps
             (mapcat #(apply range %))
             (frequencies)
             (max-by second))]
    {:guard-id  guard-id
     :minute    minute
     :frequency frequency}))

(defn guard-id*minute
  [{:keys [guard-id minute]}]
  (* guard-id minute))

(defn records
  []
  (with-open [reader (io/reader input)]
    (->> (line-seq reader)
         (map parse-record)
         (sort-by datetime->str)
         (partition-by-shift)
         (map sleeps)
         (group-by :guard-id)
         (vals)
         (map merge-grouped-records))))

(defn solve-1
  []
  (->> (records)
       (max-by total-sleep-length)
       (most-sleeping-minute)
       (guard-id*minute)))

(defn solve-2
  []
  (->> (records)
       (map most-sleeping-minute)
       (max-by :frequency)
       (guard-id*minute)))
