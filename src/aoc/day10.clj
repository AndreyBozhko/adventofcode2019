(ns aoc.day10
  (:require [clojure.string :as str]))


(def ^:const target 200)
(def asteroid? (partial = \#))

(defn read-input [path]
  (->> path
       (slurp)
       (str/split-lines)
       (map (partial keep-indexed #(when (asteroid? %2) %1)))
       (keep-indexed #(interleave %2 (repeat %1)))
       (flatten)
       (partition 2)
       (map (fn [[x y]] {:x x :y y}))))


(defn best-location [input]
  (->> input
       (map (fn [x1y1]
              (map (fn [x2y2]
                     (merge-with - x2y2 x1y1))
                   input)))
       (map (fn [coords]
              (reduce (fn [coll {:keys [x y]}]
                        (conj coll (Math/atan2 x y)))
                      #{}
                      coords)))
       (map count)
       (interleave input)
       (partition 2)
       (reduce (fn [coll [k v]] (assoc coll k v)) {})
       (apply max-key val)))


(defn nth-asteroid [best target input]
  (let [grouped (->> input
                     (map (fn [xy] (merge-with - xy best)))
                     (reduce (fn [coll {:keys [x y] :as xy}]
                               (conj coll (merge xy
                                                 {:t (Math/atan2 x y)
                                                  :r (+ (* x x) (* y y))})))
                             [])
                     (group-by :t)
                     (map (fn [[k v]] [k (sort-by :r v)]))
                     (into {}))
        ks      (sort > (keys grouped))]

    (loop [i    1
           kseq (cycle ks)
           g    grouped]

      (let [t   (first kseq)
            lst (get g t)]

        (if (and (= target i)
                 (seq lst))
          (merge-with + best (first lst))
          (recur (if (seq lst) (inc i) i)
                 (next kseq)
                 (if (seq lst) (assoc g t (next lst)) g)))))))


(defn solve [path]

  (let [input         (read-input path)
        sol1          (best-location input)
        {:keys [x y]} (nth-asteroid (first sol1) target input)]

    ;; part 1
    (println (second sol1))

    ;; part 2
    (println (+ (* 100 x) y))))
