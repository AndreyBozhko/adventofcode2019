(ns aoc.day04
  (:require [clojure.string :as str]))


(defn read-input [path]
  (as-> path $
        (slurp $)
        (str/split $ #"-")
        (map #(Long/parseLong %) $)))


(defn check [group-sizes num]
  (let [v (into [] (str num))
        s (set (vals (frequencies v)))]
    (and (apply <= (map int v))
         (some s group-sizes))))


(defn count-passwords [group-sizes [lo hi]]
  (count
    (filter (partial check group-sizes) (range lo hi))))


(defn solve [path]

  (let [input (read-input path)
        ans-1 (count-passwords (range 2 7) input)
        ans-2 (count-passwords (range 2 3) input)]

    (println ans-1)
    (println ans-2)))
