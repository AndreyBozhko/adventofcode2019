(ns aoc.day02
  (:require [clojure.string :as str]))

(def target 19690720)
(def funcs {1 + 2 *})


(defn read-input [path]
  (as-> path $
        (slurp $)
        (str/split $ #",")
        (map #(Long/parseLong %) $)
        (zipmap (range) $)))


(defn run-program [i m]
  (let [op (get m i)
        l  (get m (+ 1 i))
        r  (get m (+ 2 i))
        t  (get m (+ 3 i))]
    (if (= 99 op)
      m
      (recur (+ 4 i)
             (assoc m t ((get funcs op)
                           (get m l)
                           (get m r)))))))


(defn run-with-pair [pair instruction]
  (let [noun (long (/ pair 100))
        verb (- pair (* 100 noun))
        inst (merge instruction {1 noun 2 verb})]
    (get (run-program 0 inst) 0)))


(defn solve-a [inst]
  (run-with-pair 1202 inst))

(defn solve-b [inst]
  (loop [pair 0]
    (when (> 10000 pair)
      (if (= target (run-with-pair pair inst))
        pair
        (recur (inc pair))))))


(defn solve [path]

  (let [input (read-input path)
        ans-1 (solve-a input)
        ans-2 (solve-b input)]

    (println ans-1)
    (println ans-2)))
