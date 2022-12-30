(ns aoc.day25
  (:require [clojure.string :as str]
            [aoc.day05 :refer [exit-codes
                               read-input
                               run-program
                               program-reads-input
                               program-prints-output]]))


;; Correct set of items:
;; - coin
;; - festive hat
;; - mutex
;; - whirled peas


(defn solver
  "Run program defined by state 'st' given a list 'inp-list' of input commands" 
  [inp-list st]
  (let [inp-stack (atom inp-list)
        out-stack (atom '())]

    (with-redefs [;; override program input - input will be read from inp-stack instead of waiting for user input
                  program-reads-input   (fn []
                                          (when (empty? @inp-stack) (swap! exit-codes conj 3))
                                          (ffirst (swap-vals! inp-stack next)))

                  ;; override program output - output will be added to out-stack instead of printing
                  program-prints-output (fn [x]
                                          (if (= x 10)
                                            (do
                                              (println (str/join "" (map char (reverse @out-stack))))
                                              (reset! out-stack '()))
                                            (swap! out-stack conj x))
                                          nil)]

      (run-program st))))


(defn operate-droid [state]
  (loop [st  state
         inp '()]

    (let [new-st  (solver inp st)
          new-st  (merge-with - new-st {:i 2})
          _       (swap! exit-codes disj 3)
          new-inp (->> (read-line)
                       (reverse)
                       (map int)
                       (into '(10)))]

      (recur new-st new-inp))))


(defn solve [path]
  (->> path
       (read-input)
       (assoc {:i 0 :o (atom 0)} :m)
       (operate-droid)))
