(ns sicp.huffman)

(defrecord Node [symbols weight left right])

(defn leaf [symbol weight]
  (Node. #{symbol} weight nil nil))

(defn union [a b]
  (into a b))

(defn tree [left right]
  (Node. (union (:symbols left) (:symbols right))
         (+ (:weight left) (:weight right))
         left
         right))

(defn leaf? [x]
  (and (instance? Node x)
       (nil? (:right x))
       (nil? (:left x))))

(defn tree? [x]
  (and (instance? Node x)
       (or (:right x)
           (:left x))))

(def symbols :symbols)

(def weight :weight)

(def right :right)

(def left :left)

(defn decoder [tree]
  (letfn [(choose-branch [bit branch]
            ((if (zero? bit)
               left
               right)
             branch))
          (decode-loop [bits branch output]
            (if bits
              (let [next-branch (choose-branch (first bits) branch)]
                (if (leaf? next-branch)
                  (recur (next bits)
                         tree
                         (conj output
                               (first (symbols next-branch))))
                  (recur (next bits)
                         next-branch
                         output)))
              output))]
    (fn decode [bits]
      (decode-loop (seq bits) tree []))))

(defn decode [bits tree]
  ((decoder tree) bits))

(defn sorted-set-by-keyfn [f & keys]
  (apply sorted-set-by #(compare (-> %1 f vec)
                                 (-> %2 f vec))
         keys))

(defn make-leaf-set [pairs]
  (sort-by weight
           (for [[s w] pairs]
             (leaf s w))))

(defn insert [node leaf-set]
  (if leaf-set
    (if (< (weight (first leaf-set))
           (weight node))
      (cons (first leaf-set)
            (insert node (next leaf-set)))
      (cons node leaf-set))
    (list node)))

(def sample-tree
     (tree (leaf \A 4)
           (tree (leaf \B 2)
                 (tree (leaf \D 1)
                       (leaf \C 1)))))

(def sample-message [0 1 1 0 0 1 0 1 0 1 1 1 0])

(defn encode-symbol [sym tree]
  (assert ((symbols tree) sym))
  (loop [branch tree, output []]
    (if (leaf? branch)
      output
      (if ((symbols (left branch)) sym)
        (recur (left branch) (conj output 0))
        (recur (right branch) (conj output 1))))))

(defn encoder [tree]
  (fn encode [message]
    (loop [m message, output []]
      (if m
        (recur (next m)
               (into output
                     (encode-symbol (first m)
                                    tree)))
        output))))

(defn encode [message tree]
  ((encoder tree) message))

(defn successive-merge [nodes]
  (cond (= 1 (count nodes)) (first nodes)
        (= 2 (count nodes)) (apply tree nodes)
        :else (let [[a b & more] nodes]
                (recur (insert (tree a b) more)))))

(def fifties-rock-lyric-counts
     (quote [[a 2]
             [boom 1]
             [get 2]
             [job 2]
             [na 16]
             [sha 3]
             [yip 9]
             [wah 1]]))

(def fifties-lyric
     (quote [get a job
             sha na na na na na na na
             get a job
             sha na na na na na na na
             wah yip yip yip yip yip yip yip yip yip
             sha boom]))

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))

(defn indent [n]
  (apply str (repeat n " ")))

(defn print-tree
  ([t] (print-tree t 0))
  ([t n] (if (leaf? t)
           (println (indent n) (weight t) (first (symbols t)))
           (do (println (indent n) (weight t))
               (print-tree (left t) (inc n))
               (print-tree (right t) (inc n))))))


