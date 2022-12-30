(ns aoc.day13
  (:require [aoc.day05 :refer [exit-codes
                              read-input
                              run-program
                              program-reads-input
                              program-prints-output]]))


;; exit program on opcode 4 as well
(swap! exit-codes conj 4)


(defn solver [input]
  (let [grid       (atom {})
        score      (atom nil)
        ball       (atom nil)
        bat        (atom nil)
        stack      (atom '())
        joystick   (atom 0)
        ;; initialize program state
        state      (atom {:i 0
                          :m input
                          :o (atom 0)})
        ;; helper function to check program output added to stack
        read-stack (fn [] (ffirst (swap-vals! stack next)))]

    (with-redefs [;; override program input - input will be the position of joystick
                  program-reads-input   (fn [] @joystick)
                  ;; override program output - output will be added to stack instead of printing
                  program-prints-output (fn [x] (swap! stack conj x) nil)]


      (loop [j 0]
        (let [{i   :i
               m   :m
               :as exit-state} (run-program @state)]

          ;; save state of program
          (swap! state merge exit-state)
          (when (= 3 (count @stack))
            ;; read three outputs from stack
            (let [t (read-stack)
                  y (read-stack)
                  x (read-stack)]
              (if (and (= -1 x) (= 0 y))
                ;; update score
                (reset! score t)
                ;; update grid
                (case t
                  0 (swap! grid dissoc [x y])
                  1 (swap! grid assoc [x y] t)
                  2 (swap! grid assoc [x y] t)
                  3 (reset! bat {:x x :y y})
                  4 (do (reset! ball {:x x :y y})
                        (reset! joystick (compare (:x @ball) (:x @bat))))))))

          ;; repeat until done
          (when-not (= 99 (get m i))
            (recur (inc j)))))

      ;; return grid and score
      {:grid  @grid
       :score @score})))


(defn solve [path]

  (let [input (read-input path)]

    ;; part 1 - run as is
    (->> (solver input)
         (:grid)
         (vals)
         (filter (partial = 2))
         (count)
         (println "Number of block tiles:"))

    ;; part 2 - set memory address 0 to 2
    (->> (solver (merge input {0 2}))
         (:score)
         (println "Final score:"))))
