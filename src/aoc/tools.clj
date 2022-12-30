(ns aoc.tools)


(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s]
                      (map (partial cons x) (permutations (remove #{x} s)))))
      [s])))

;; From: https://github.com/clojure/math.combinatorics/
(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
                        (fn step [v-seqs]
                          (let [increment
                                (fn [v-seqs]
                                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                                    (if (= i -1) nil
                                                 (if-let [rst (next (v-seqs i))]
                                                   (assoc v-seqs i rst)
                                                   (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
                            (when v-seqs
                              (cons (map first v-seqs)
                                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))


(defn topo-sort [edges]
  (loop [stack '()]
    (let [seen      (set stack)
          new-stack (reduce (fn [coll [k v]]
                              (if (and (nil? (seen k))
                                       (or (empty? v)
                                           (not-any? nil? (map seen v))))
                                (conj coll k)
                                coll))
                            stack
                            edges)]

      (if (= (count seen) (count (keys edges)))
        new-stack
        (recur new-stack)))))


(defn partition-by-list
  "Example usage:
   (partition-by-list
     '(2 3)
     '(1 2 3 4 5 2 3 1 4 5 1))
   => ((1) (2 3) (4 5) (2 3) (1 4 5 1))"
  [lst coll]
  (loop [tmp '()
         res '()
         cl  coll]
    (let [flag (= lst (take (count lst) cl))]
      (if (empty? cl)
        ;; return
        (->> (reverse tmp) (conj res) (reverse) (filter not-empty))
        ;; else
        (if flag
          (recur '()
                 (conj res (reverse tmp) lst)
                 (drop (count lst) cl))
          (recur
            (conj tmp (first cl))
            res
            (next cl)))))))

(defn xgcd [AA BB]
  (loop [[a b] [AA BB]
         [x prevx] [1 0]
         [y prevy] [0 1]]

    (if (zero? b)
      [a prevx prevy]

      (let [q (quot a b)]
        (recur [b (mod a b)]
               [(- prevy (* q y)) y]
               [(- prevx (* q x)) x])))))
