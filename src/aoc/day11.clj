(ns aoc.day11
  (:require [clojure.string :as str]
            [aoc.day05 :refer [exit-codes
                              read-input
                              run-program
                              program-reads-input
                              program-prints-output]]))


;; exit program on opcode 4 as well
(swap! exit-codes conj 4)


(defn move-robot [robot turn]
  (let [turns {:N {0 :W 1 :E}
               :S {0 :E 1 :W}
               :E {0 :N 1 :S}
               :W {0 :S 1 :N}}
        moves {:N {:x 0 :y 1}
               :S {:x 0 :y -1}
               :E {:x 1 :y 0}
               :W {:x -1 :y 0}}
        dir   (get-in turns [(:d robot) turn])]
    (-> robot
        ((partial merge-with +) (get moves dir))
        (assoc :d dir))))


(defn solver [start input]
  (let [grid       (atom {[0 0] start})
        robot      (atom {:x 0 :y 0 :d :N})
        stack      (atom '())
        ;; initialize robot state
        state      (atom {:i 0
                          :m input
                          :o (atom 0)})]

    (with-redefs [;; override program input - input will be read from stack instead of waiting for user input
                  program-reads-input   (fn [] (ffirst (swap-vals! stack next)))
                  ;; override program output - output will be added to stack instead of printing
                  program-prints-output (fn [x] (swap! stack conj x) nil)]


      (loop [j 0]

        ;; push color of the current cell to stack
        (when (empty? @stack)
          (program-prints-output (get @grid [(:x @robot) (:y @robot)] 0)))

        (let [{i   :i
               m   :m
               :as exit-state} (run-program @state)]

          ;; save state of program and robot
          (swap! state merge exit-state)
          (when (= 2 (count @stack))
            ;; color and direction output in stack present in stack
            (let [t (program-reads-input)
                  c (program-reads-input)]
              (swap! grid assoc [(:x @robot) (:y @robot)] c)
              (reset! robot (move-robot @robot t))))

          ;; repeat until done
          (when-not (= 99 (get m i))
            (recur (inc j)))))

      ;; return painted grid
      @grid)))


(defn solve [path]

  (let [input (read-input path)]

    ;; part 1 - provide 0 as starting color
    (println (count (keys (solver 0 input))))

    ;; part 2 - provide 1 as starting color
    (let [grid    (solver 1 input)
          [x1 x2] (apply (juxt min max) (map first (keys grid)))
          [y1 y2] (apply (juxt min max) (map second (keys grid)))
          res  (map
                 (fn [y]
                   (->> (range x1 (inc x2))
                        (map #(get grid [% y] 0))
                        (map #(if (zero? %) " " "#"))
                        (str/join "")))
                 (range y1 (inc y2)))]
      (println (str/join "\n" (reverse res))))))
