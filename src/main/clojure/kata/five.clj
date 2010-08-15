(ns kata.five                           ; bloom filter
  (:use clojure.java.io)
  (:import java.security.MessageDigest))

(def +number-of-words+ 1000)

(def words
     (apply vector
            (->> "kata/five/words"
                resource
                reader
                line-seq
                (filter seq)
                (take +number-of-words+))))

(def word? (set words))

(def *algorithm* "SHA-256")

(defn hash-bytes
  [word] 
  (seq (.digest (MessageDigest/getInstance *algorithm*)
                (.getBytes word))))

(defn hash-byte-seq
  [word]
  (apply concat (for [i (iterate inc 0)]
                  (hash-bytes (str word "\0" i)))))

(defn hash-numbers
  ([word hash-max]
     (let [nbytes (int (Math/ceil (/ (Math/log hash-max)
                                     (Math/log 256))))]
       (map #(mod (BigInteger. 1 (byte-array %))
                  hash-max)
            (partition nbytes
                       (hash-byte-seq word)))))
  ([word hash-max hash-count]
     (take hash-count
           (hash-numbers word hash-max))))

(defn bloom-filter
  [hash-max hash-count]
  {:bits BigInteger/ZERO
   :hash-max hash-max
   :hash-count hash-count
   :n 0})

(defn bloom-conj
  [bloom word]
  (let [{:keys [bits hash-max hash-count n]} bloom]
    (assoc bloom
      :bits (reduce bit-set
                    bits
                    (hash-numbers word
                                  hash-max
                                  hash-count))
      :n (inc n))))

(defn bloom-stats
  [bloom]
  (dissoc
   (assoc bloom
     :density (let [bc (.bitCount (:bits bloom))
                    bl (.bitLength (:bits bloom))]
                (when-not (zero? bl)
                  (/ bc bl))))
   :bits))

(defn print-bloom-stats
  [bloom]
  (let [{:keys [n density]} (bloom-stats bloom)]
    (println "word-count" n "density" (float density))))

(defn build-bloom
  [hash-max hash-count words]
  (let [bloom (reduce bloom-conj
                      (bloom-filter hash-max hash-count)
                      words)]
    (print-bloom-stats bloom)
    bloom))

(defn bloom-contains?
  [bloom word]
  (let [{:keys [bits hash-max hash-count]} bloom]
    (every? #(bit-test bits %)
            (hash-numbers word
                          hash-max
                          hash-count))))

(defn some-word
  []
  (words (rand-int (count words))))

(defn mutate
  [word]
  (apply str (shuffle (seq word))))

(defn invent-word
  []
  (mutate (some-word)))

(defn invented-words
  []
  (repeatedly invent-word))


(defn rates
  [hash-max hash-count iteration-count]
  (let [bloom (build-bloom hash-max hash-count words)]
    (loop [ws (take iteration-count (invented-words))
           positive 0
           false-positive 0
           negative 0]
      (if ws
        (let [w (first ws)]
          (if (bloom-contains? bloom w)
            (if (word? w)
              (recur (next ws)
                     (inc positive)
                     false-positive
                     negative)
              (recur (next ws)
                     positive
                     (inc false-positive)
                     negative))
            (do
              (assert (not (word? w))) ; bloom filters don't false negative
              (recur (next ws)
                     positive
                     false-positive
                     (inc negative)))))
        {:positive positive
         :false-positive false-positive
         :negative negative}))))











