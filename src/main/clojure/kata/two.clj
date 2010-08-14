(ns kata.two
  (import java.util.NoSuchElementException))

(defn chop0
  "this implementation isn't binary search, it's just a smoke test for
the test."
  [e v]
  (first (keep-indexed #(when (= e %2) %1) v)))

(defn chop1
  "interative, indexed, using a half-open range"
  [e v]
  (loop [a (int 0) b (count v)]
    (when (< a b)
      (let [i (int (/ (+ a b) 2))]
        (cond (= (v i) e) i
              (< (v i) e) (recur (inc i) b)
              :else       (recur a       i))))))

(defn chop2
  "iterative, using sub-vectors"
  [e v]
  (loop [w v b 0]
    (when (seq w)
      (let [i (int (/ (count w) 2))]
        (cond (= (w i) e) (+ i b)
              (< (w i) e) (recur (subvec w (inc i)) (+ 1 i b))
              :else       (recur (subvec w 0 i) b))))))

(defn chop3
  "recursive, using sub-vectors"
  [e v]
  (when (seq v)
    (let [i (int (/ (count v) 2))]
      (cond (< (v i) e) (when-let [j (chop3 e (subvec v (inc i)))]
                          (+ 1 i j))
            (> (v i) e) (recur e (subvec v 0 i))
            :else i))))

(defn chop4
  "a variation on recursive, using sub-vectors.
use exception internally to signal not found."
  [e v]
  (letfn [(split [v]
            (let [i (int (/ (count v) 2))]
              [(delay (subvec v 0 i))
               i (v i)
               (delay (subvec v (inc i)))]))
          (chop [v]
            (if (seq v)
              (let [[lesser at value greater] (split v)]
                (cond (< e value) (recur (force lesser))
                      (> e value) (+ 1 at (chop (force greater)))
                      :else at))
              (throw (NoSuchElementException.))))]
    (try
      (chop v)
      (catch NoSuchElementException _ nil))))


(defn chop5
  "interative, indexed, using a closed range"
  [e v]
  (loop [a (int 0) b (dec (count v))]
    (when (<= a b)
      (let [i (int (/ (+ a b) 2))]
        (cond (= (v i) e) i
              (< (v i) e) (recur (inc i) b)
              :else       (recur a       (dec i)))))))
