(ns aoc.day06
  (:require [clojure.string :as str]))


(def COM '("COM"))
(def YOUSAN '("YOU" "SAN"))


(defn read-input [path]
  (->> path
       (slurp)
       (str/split-lines)
       (map #(str/split % #"\)"))))


(defn count-orbits [gr]
  (loop [obj COM
         cur 1
         cnt 0]
    (let [st (mapcat gr obj)]
      (if (empty? st)
        cnt
        (recur st
               (inc cur)
               (+ cnt (* (count st) cur)))))))


(defn count-transfers [gr]
  (loop [obj  YOUSAN
         cur  0
         path {}]
    (let [st (map gr obj)]
      (cond
        (apply = st) (* 2 cur)
        (some path st) (+ cur (some path st))
        :else
        (recur st
               (inc cur)
               (merge path
                      (zipmap
                        (remove nil? st)
                        (repeat cur))))))))


(defn solve [path]

  (let [gr (->> path
                (read-input)
                (group-by first)
                (map (fn [[k v]] [k (map second v)]))
                (into {}))]
    (println (count-orbits gr)))

  (let [rg (->> path
                (read-input)
                (map reverse)
                (reduce #(apply assoc %1 %2) {}))]
    (println (count-transfers rg))))
