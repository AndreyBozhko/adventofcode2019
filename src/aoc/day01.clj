(ns aoc.day01
  (:require [clojure.string :as str]))


(defn read-input [path]
  (->> path
       (slurp)
       (str/split-lines)
       (map #(Long/parseLong %))))


(defn single-transform [x]
  (max 0 (- (long (/ x 3)) 2)))


(def transform-a single-transform)

(defn transform-b [x]
  (loop [l (conj '() x)]
    (if (zero? (first l))
      (- (apply + l) x)
      (recur (conj l (single-transform (first l)))))))


(defn calculate [transform input]
  (->> input
       (map transform)
       (apply +)))


(defn solve [path]

  (let [input (read-input path)
        ans-1 (calculate transform-a input)
        ans-2 (calculate transform-b input)]

    (println ans-1)
    (println ans-2)))
