(ns aoc.core
  (:require [clojure.string :as str]))


(def aoc-days
  (->> (range 1 26)
       (map (partial format "day%02d"))
       (set)))


(defn -main [& args]

  (let [day        (first args)
        input-path (format "resources/input%s" (str/join (drop 3 day)))]

    ;; assert that the day is one of the allowed values
    (assert (string? day))
    (assert (contains? aoc-days day) "Wrong day of Advent-of-Code provided!")

    (let [;; specify the namespace which contains solutions for the day
          ns-to-run (str "aoc." day)
          ;; load and resolve the solve function from that namespace
          _         (require (symbol ns-to-run))
          solve     (resolve (symbol ns-to-run "solve"))]
      ;; run the solution
      (solve input-path))))
