(ns aoc22.day11
  (:require [clojure.java.io :as io]
            [aoc22.shared :as shared]))

(def input "./data/aoc22/day11.txt")

(defn parse-operation
  [s]
  (let [[a op b] (clojure.string/split s #" ")]
    [op a b]))

(defn parse
  [[index items operation test success error]]
  {:index     (-> (re-matches #"Monkey ([\d]+):" index) (nth 1) parse-long)
   :items     (->> (clojure.string/split (subs items 18) #", ")
                   (mapv parse-long))
   :operation (parse-operation (subs operation 19))
   :test      (parse-long (subs test 21))
   :success   (parse-long (subs success 29))
   :error     (parse-long (subs error 30))
   ;
   :inspected 0})

(defn op
  [[op a b] item]
  ((if (= op "+") + *)
   (if (= a "old") item (parse-long a))
   (if (= b "old") item (parse-long b))))

(defn round
  [monkeys f state]
  (doseq [{:keys [index operation test success error]} monkeys]
    (doseq [item (get-in @state [index :items])]
      (let [s (f (op operation item))]
        (vswap! state update-in [(if (zero? (mod s test)) success error) :items] conj s)
        (vswap! state update-in [index :items] #(->> % (drop 1) vec))
        (vswap! state update-in [index :inspected] inc)))))

(defn solve-1
  []
  (with-open [reader (io/reader input)]
    (let [monkeys (into []
                        (comp (shared/section-seq identity)
                              (map parse))
                        (line-seq reader))
          state (volatile! monkeys)
          _ (dotimes [_ 20]
              (round monkeys #(int (/ % 3)) state))]
      (->> @state
           (map :inspected)
           (sort >)
           (take 2)
           (reduce *)))))

(defn solve-2
  []
  (with-open [reader (io/reader input)]
    (let [monkeys (into []
                        (comp (shared/section-seq identity)
                              (map parse))
                        (line-seq reader))
          super-modulo (->> monkeys (map :test) (reduce *))
          state (volatile! monkeys)
          _ (dotimes [_ 10000]
              (round monkeys #(mod % super-modulo) state))]
      (->> @state
           (map :inspected)
           (sort >)
           (take 2)
           (reduce *)))))
