(ns aoc.day21
  (:require [clojure.string :as str]
            [aoc.day05 :refer [read-input
                               run-program
                               program-reads-input
                               program-prints-output]]))


(defn ascii-fy [instr]
  (map int (str/join "" (interleave instr (repeat "\n")))))

(defn stringify [instr]
  (str/join "" (map #(try (char %) (catch Exception _ %)) instr)))


(defn solver
  "Run program defined by state 'st' given a list 'inp-list' of input commands"
  [inp-list st]
  (let [inp-stack (atom inp-list)
        out-stack (atom '())]

    (with-redefs [;; override program input - input will be read from inp-stack instead of waiting for user input
                  program-reads-input   (fn [] (ffirst (swap-vals! inp-stack next)))
                  ;; override program output - output will be added to out-stack instead of printing
                  program-prints-output (fn [x] (swap! out-stack conj x) nil)]

      (run-program st)
      @out-stack)))


(defn solve [path]

  (let [st    {:i 0
               :o (atom 0)
               :m (read-input path)}

        ;; part 1 - walking
        ;; if   D is ground,
        ;;      AND any of A, B, C is hole,
        ;; then jump to D
        instr ["NOT T T"
               "AND A T"
               "AND B T"
               "AND C T"
               "NOT T J"
               "AND D J"
               "WALK"]

        ans1  (solver (ascii-fy instr) st)

        ;; part 2 - running
        ;; if   D is ground,
        ;;      AND any of A, B, C is hole,
        ;;      AND any of E, H is ground
        ;; then jump to D
        instr ["NOT T T"
               "AND A T"
               "AND B T"
               "AND C T"
               "NOT T J"
               "AND D J"
               "NOT H T"
               "NOT T T"
               "OR E T"
               "AND T J"
               "RUN"]
        ans2  (solver (ascii-fy instr) st)]

    (println "Part 1:")
    (println (stringify (reverse ans1)))

    (println "\n\n")

    (println "Part 2:")
    (println (stringify (reverse ans2)))))
