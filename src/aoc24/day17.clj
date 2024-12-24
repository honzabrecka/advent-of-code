(ns aoc24.day17)

(defn mod8
  [n]
  (mod n 8))

(defn next-i
  [i]
  (+ i 2))

(defn divide-pow
  [a b]
  (long (/ a (Math/pow 2 b))))

(defn run
  ([register instructions]
   (run register instructions false))
  ([registers instructions check?]
   (loop [registers registers
          output []
          i 0]
     (let [opcode (get instructions i)
           operand (get instructions (inc i))
           operand' (case operand
                      4 (get registers :a)
                      5 (get registers :b)
                      6 (get registers :c)
                      7 (throw (Error. "Invalid operand"))
                      operand)]
       (if (or (nil? opcode) (nil? operand))
         [:halt registers output]
         (case opcode
           0 (recur (update registers :a #(divide-pow % operand')) output (next-i i))
           1 (recur (update registers :b #(bit-xor % operand)) output (next-i i))
           2 (recur (assoc registers :b (mod8 operand')) output (next-i i))
           3 (if (zero? (get registers :a))
               (recur registers output (next-i i))
               (recur registers output operand))
           4 (recur (update registers :b #(bit-xor % (get registers :c))) output (next-i i))
           5 (let [output' (conj output (mod8 operand'))]
               (if (and check? (not= (take (count output') instructions) output'))
                 [:halt registers output']
                 (recur registers output' (next-i i))))
           6 (recur (assoc registers :b (divide-pow (get registers :a) operand')) output (next-i i))
           7 (recur (assoc registers :c (divide-pow (get registers :a) operand')) output (next-i i))))))))

(defn result
  [[_ registers output]]
  (println registers)
  (clojure.string/join "," output))

(defn solve-1
  []
  (result (run {:a 729 :b 0 :c 0}
               [0,1,5,4,3,0])))

(defn solve-1
  []
  (result (run {:a 17323786 :b 0 :c 0}
               [2,4,1,1,7,5,1,5,4,1,5,5,0,3,3,0])))

(defn solve-2
  []
  (result (run {:a 117440 :b 0 :c 0}
               [0,3,5,4,3,0]
               true)))
