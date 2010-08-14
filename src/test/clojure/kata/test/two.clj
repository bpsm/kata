(ns kata.test.two
  (:use clojure.test)
  (:use kata.two))

(deftest test-chop-functions
  (are [chop]
       (testing (str "the given: " chop)
         (are [v] (= nil (chop 3 v))
              [], [1])
         (are [r e] (= r (chop e [1 3 5]))
              0 1, 1 3, 2 5
              nil 0, nil 2, nil 4, nil 6)
         (are [r e] (= r (chop e [1 3 5 7]))
              0 1, 1 3, 2 5, 3 7
              nil 0, nil 2, nil 4, nil 6, nil 8))
       chop0
       chop1
       chop2
       chop3
       chop4
       chop5))



