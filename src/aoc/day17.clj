(ns aoc.day17
  (:require [clojure.string :as str]
            [aoc.tools :refer [cartesian-product
                               partition-by-list]]
            [aoc.day05 :refer [read-input
                               run-program
                               program-reads-input
                               program-prints-output]]))


(def ^:const r-names ["A" "B" "C"])
(def ^:const inst-limit 10)

(def dxdy
  {\0 {:x 0 :y 0}
   \^ {:x 0 :y -1}
   \v {:x 0 :y 1}
   \< {:x -1 :y 0}
   \> {:x 1 :y 0}})


(defn get-grid
  "Takes program output 'stack'
   returns 2D grid {{:x 0 :y 0} \\#
                    {:x 1 :y 0} \\.
                    ...}"
  [stack]
  (->> stack
       (reverse)
       (map char)
       (apply str)
       (str/split-lines)
       (map-indexed (fn [y line]
                      (map-indexed (fn [x c]
                                     [{:x x :y y} c])
                                   line)))
       (mapcat identity)
       (into {})))


(defn get-alignment
  "Takes 2D grid {{:x 0 :y 0} #
                  {:x 1 :y 0} .
                  ...}
   returns sum of x*y for all the scaffolds that are at the intersection"
  [grid]
  (->> grid
       (filter (fn [[xy _]]
                 (every? (partial = \#) (map grid
                                             (map (partial merge-with +)
                                                  (vals dxdy)
                                                  (repeat xy))))))
       (map (fn [[{:keys [x y]} _]] (* x y)))
       (apply +)))


(defn get-path
  "Takes 2D grid {{:x 0 :y 0} #
                  {:x 1 :y 0} .
                  ...}
   returns list of instructions '(R 10 L 8 ...)"
  [grid]
  (let [[start d] (->> grid
                       (filter (fn [[_ c]] (nil? (#{\. \#} c))))
                       (first))]
    (loop [cur  start
           dir  (dxdy d)
           path '()]

      (let [new-dir (->> (vals dxdy)
                         (remove (partial = (dxdy \0)))
                         (remove (partial = (merge-with - (dxdy \0) dir)))
                         (keep #(when (= \# (grid (merge-with + cur %))) %)))
            new-dir (if (< 1 (count new-dir)) dir (first new-dir))

            trn     (when new-dir
                      (- (* (:x new-dir) (:y dir))
                         (* (:y new-dir) (:x dir))))

            new-cur (if (= dir new-dir)
                      (merge-with + cur dir)
                      cur)
            step    (cond
                      (nil? new-dir) nil
                      (= dir new-dir) 1
                      (pos? trn) "L"
                      (neg? trn) "R")]

        (if (nil? new-dir)
          (->> path
               (reverse)
               (partition-by number?)
               (map #(if (number? (first %)) (apply + %) (first %))))
          (recur new-cur new-dir (conj path step)))))))


(defn get-routines
  "Given 'r-names'=['A' 'B' 'C'], limit on each instruction 'inst-limit', and 'path'=(R 10 L 8 ...)
   returns [ ((A) (B) (A) (C) ...)
             {A (R 10 L 8 ...) B (L 6 L 6 ...) C (L 6 R 12 ...)} ]"
  [r-names inst-limit path]
  (loop [lAlBlC (apply cartesian-product
                       (repeat (count r-names)
                               (range 1 (inc inst-limit))))]

    (let [ls    (zipmap r-names (first lAlBlC))
          parts (reduce (fn [coll [Q l]]
                          (let [[pth-reduced pth-replaced ABC] (first coll)
                                abc (take l (first pth-reduced))]
                            (conj coll
                                  [(remove (partial = abc)
                                           (mapcat (partial partition-by-list abc) pth-reduced))
                                   (map #(if (= % abc) (list Q) %)
                                        (mapcat (partial partition-by-list abc) pth-replaced))
                                   (assoc ABC Q abc)])))
                        (list [(list path) (list path) {}])
                        ls)]

      (if (or (empty? lAlBlC)
              (empty? (ffirst parts)))
        (nfirst parts)
        (recur (next lAlBlC))))))


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

  (let [input  (read-input path)

        ;; part 1
        ;; run program
        stack1 (solver '()
                       {:i 0
                        :o (atom 0)
                        :m input})
        ;; parse program output into 2D grid
        grid   (get-grid stack1)
        ;; compute alignments
        align  (get-alignment grid)

        ;; part 2
        ;; construct a path from 2D grid
        path   (get-path grid)
        ;; split path into routines
        [sq rts] (get-routines r-names inst-limit path)
        ;; convert routines into program instructions
        sol    (-> (list (flatten sq))
                   (into (map rts r-names))
                   ;; choose 'no' for the last instruction
                   (conj (list "n"))
                   (reverse))
        sol    (->> sol
                    (map (partial str/join ","))
                    (str/join "\n")
                    (map int))
        ;; run program with modified input
        stack2 (solver sol
                       {:i 0
                        :o (atom 0)
                        :m (merge input {0 2})})
        ;; get resulting score
        score  (first stack2)]

    (println "Part 1:" align)
    (println "Part 2:" score)))
