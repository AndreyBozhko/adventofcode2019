(ns aoc.day22
  (:require [clojure.string :as str]
            [aoc.tools :refer [xgcd]]))

(def ^:const card1 2019)
(def ^:const length1 10007)

(def ^:const card2 2020)
(def ^:const length2 119315717514047)
(def ^:const n-shuffles 101741582076661)


(defn read-input [path]
  (->> path
       (slurp)
       (str/split-lines)))


(defn compile-techniques [{:keys [techniques modulo]}]
  (first
    (reduce (fn [coll technique]
              (let [[a b] (first coll)
                    cut   (second (re-matches #"cut (-?\d+)" technique))
                    deal  (second (re-matches #"deal with increment (\d+)" technique))
                    stack (re-matches #"deal into new stack" technique)

                    [a b] (cond
                            cut [a (- b (Long/parseLong cut))]
                            deal (mapv (partial * (Long/parseLong deal)) [a b])
                            stack [(- a) (- (inc b))])]

                (conj coll (mapv #(mod % modulo) [a b]))))
            (list [1 0])
            techniques)))


(defn combine-shuffles

  ([modulo shuffle]
   (mapv #(mod % modulo) shuffle))

  ([modulo [a1 b1] [a2 b2]]
   (->> [(*' a1 a2) (+' b2 (*' a2 b1))]
        (mapv #(mod % modulo))
        (mapv long)))

  ([modulo r l & more]
   (if (seq more)
     (apply combine-shuffles modulo (combine-shuffles modulo r l) more)
     (combine-shuffles modulo r l))))


(defn shuffle-pow [{:keys [shuffle power modulo]}]
  (let [bstr   (Long/toBinaryString power)
        powers (reduce (fn [coll _]
                         (let [[a b] (first coll)]
                           (conj coll
                                 (combine-shuffles modulo [a b] [a b]))))
                       (list shuffle)
                       (range 1 (count bstr)))]
    (->> powers
         (interleave bstr)
         (partition 2)
         (keep (fn [[k v]] (when (= \1 k) v)))
         (apply combine-shuffles modulo))))


(defn apply-shuffle [{:keys [card shuffle modulo]}]
  (mod (+ (second shuffle) (* (first shuffle) card)) modulo))


(defn solve [path]

  (let [techniques    (read-input path)

        ;; part 1
        shuffle1      (compile-techniques {:techniques techniques
                                           :modulo     length1})
        ans1          (apply-shuffle {:card    card1
                                      :shuffle shuffle1
                                      :modulo  length1})

        ;; part 2
        shuffle2      (compile-techniques {:techniques techniques
                                           :modulo     length2})
        multi-shuffle (shuffle-pow {:shuffle shuffle2
                                    :power   n-shuffles
                                    :modulo  length2})

        [A B]         multi-shuffle
        _             (println (format "Need to solve %d * X + %d = %d (mod %d)" A B card2 length2))
        ;; use extended Euclidean algorithm
        [g _ q]       (xgcd length2 A)
        _             (assert (zero? (mod q g)))
        ans2          (long (mod (*' (- (/ q g)) (- B card2)) length2))]

    (println "Part 1:" ans1)
    (println "Part 2:" ans2)))
