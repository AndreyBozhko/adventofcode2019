(ns aoc.day09
  (:require [aoc.day05 :refer [read-input
                              run-program]]))


(defn solver [inst]
  (run-program {:i 0
                :m inst
                :o (atom 0)}))


(defn solve [path]

  (let [input (read-input path)]

    ;; part 1 - provide 1 as an input
    (solver input)

    ;; part 2 - provide 2 as an input
    (solver input)))
