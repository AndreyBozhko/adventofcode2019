(ns aoc.day07
  (:require [aoc.tools :refer [permutations]]
            [aoc.day05 :refer [exit-codes
                              read-input
                              run-program
                              program-reads-input
                              program-prints-output]]))


;; exit program on opcode 4 as well,
;; so can resume executing program on next amplifier
(swap! exit-codes conj 4)


(defn calculate [input states]
  (let [num-amp    (count states)
        ;; use stack to simulate IO
        ;; initialize with first instruction for first amplifier
        zero-inst  0
        stack      (atom (list zero-inst))
        ;; enumerate state for each amplifier
        s          (zipmap (range) states)
        ;; initialize amplifier states
        amp-states (reduce (fn [coll x]
                             (assoc coll x (atom {:i 0 :m input})))
                           {}
                           (range num-amp))]

    (with-redefs [;; override program input - input will be read from stack instead of waiting for user input
                  program-reads-input   (fn [] (ffirst (swap-vals! stack next)))
                  ;; override program output - output will be added to stack instead of printing
                  program-prints-output (fn [x] (swap! stack conj x) nil)]


      (loop [j 0]

        ;; push amplifier state (its first program input) to stack
        (when-let [x (get s j)] (program-prints-output x))

        (let [amp (mod j num-amp)
              {position :i
               mp       :m
               :as      exit-state} (run-program @(get amp-states amp))]

          ;; save state of amplifier
          (swap! (get amp-states amp) merge exit-state)

          ;; repeat until done
          (when-not (and (= 99 (get mp position))
                         (= num-amp (inc amp)))
            (recur (inc j)))))

      ;; return output for given combination of amplifier settings
      (first @stack))))


(defn solver [settings input]
  (let [best  (atom 0)]
    (doseq [states (permutations settings)]
      (swap! best max (calculate input states)))
    (println @best)))


(defn solve [path]

  ;; part 1
  (let [input (read-input path)]

    ;; part 1
    (solver (range 0 5) input)

    ;; part 2
    (solver (range 5 10) input)))
