(ns aoc.day15
  (:require [aoc.day05 :refer [exit-codes
                              read-input
                              run-program
                              program-reads-input
                              program-prints-output]]))


(def dxdy
  {1 {:x 0 :y 1}
   2 {:x 0 :y -1}
   3 {:x -1 :y 0}
   4 {:x 1 :y 0}})


;; exit program on opcode 4 as well
(swap! exit-codes conj 4)


(defn solver [st]
  (let [stack     (atom '())
        exit-cell (atom nil)
        ;; initialize program state
        states    {{:x 0 :y 0} {:step  0
                                :state st}}]

    (with-redefs [;; override program input - input will be read from stack instead of waiting for user input
                  program-reads-input   (fn [] (ffirst (swap-vals! stack next)))
                  ;; override program output - output will be added to stack instead of printing
                  program-prints-output (fn [x] (swap! stack conj x) nil)]


      (loop [step 1
             grid states]

        (let [process-fn (fn [[xy {:keys [state]} d]]
                           (let [;; send instruction to robot
                                 _      (program-prints-output d)
                                 ;; update coordinates
                                 new-xy (merge-with + xy (get dxdy d))
                                 ;; execute robot's program
                                 new-st (run-program state)
                                 ;; read returned code
                                 code   (program-reads-input)
                                 v      {:step  step
                                         :state new-st}]
                             ;; only if cell wasn't yet visited
                             (when (nil? (get grid new-xy))
                               (case code
                                 ;; do nothing
                                 0 nil
                                 ;; update grid
                                 1 {new-xy v}
                                 ;; update grid and exit-cell
                                 2 {new-xy (reset! exit-cell v)}))))
              ;; only take cells from previous step
              filt-grid  (->> grid
                              (into [])
                              (filter (fn [[_ v]] (= (dec step) (:step v)))))
              ;; bfs
              new-grid   (->> filt-grid
                              (mapcat (fn [kv]
                                        (mapv (partial conj kv)
                                              (keys dxdy))))
                              (map process-fn)
                              (reduce merge grid))]

          (when (empty? filt-grid)
            (reset! exit-cell (apply max-key :step (vals new-grid))))

          ;; repeat until done
          (if-not (nil? @exit-cell)
            exit-cell
            (recur (inc step) new-grid)))))))


(defn solve [path]

  (let [input (read-input path)

        ;; part 1 - run the original program
        {step1 :step
         state :state} @(solver {:i 0
                                 :m input
                                 :o (atom 0)})

        ;; part 2 - run the program from the state that
        ;; was achieved when cell with code 2 was found
        {step2 :step} @(solver state)]

    (println "Steps to reach oxygen:" step1)
    (println "Steps to fill with oxygen:" step2)))
