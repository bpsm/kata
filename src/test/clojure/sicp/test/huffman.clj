(ns sicp.test.huffman
  (:use clojure.test)
  (:use sicp.huffman))

(deftest leaves
  (let [x (leaf :x 1)]
    (is (leaf? x))
    (is (= #{:x} (symbols x)))
    (is (= 1 (weight x)))))

(deftest trees
  (let [a (leaf :a 1)
        b (leaf :b 2)
        t (tree a b)]
    (is (= a (left t)))
    (is (= b (right t)))
    (is (= 3 (weight t)))
    (is (= #{:a :b} (symbols t)))))
