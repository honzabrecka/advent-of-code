(ns aoc15.day4)

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(def input "iwrupvqb")

(defn md5
  [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(def memo-md5 (memoize md5))

(defn solve-1
  []
  (loop [i 0]
    (if (clojure.string/starts-with? (memo-md5 (str input i)) "00000")
      i
      (recur (inc i)))))

(defn solve-2
  []
  (loop [i 0]
    (if (clojure.string/starts-with? (memo-md5 (str input i)) "000000")
      i
      (recur (inc i)))))

#_(re-find #"(.)\1{1,}" "abccd")
