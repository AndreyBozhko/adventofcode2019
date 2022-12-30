(ns aoc.day05
  (:require [clojure.string :as str]))


(defn program-reads-input []
  (print "Input required: ")
  (flush)
  (Long/parseLong (read-line)))

(defn program-prints-output [x]
  (println "Output:" x))


(def exit-codes (atom #{99}))

(def steps
  (atom {1  4
         2  4
         3  2
         4  2
         5  3
         6  3
         7  4
         8  4
         9  2
         99 0}))


(def funcs
  (atom {1  (fn [[l r] & _] {:m (+ l r)})
         2  (fn [[l r] & _] {:m (* l r)})
         3  (fn [& _]       {:m (program-reads-input)})
         4  (fn [[l] & _]   (program-prints-output l))
         5  (fn [[l r] & _] {:i (when-not (zero? l) r)})
         6  (fn [[l r] & _] {:i (when (zero? l) r)})
         7  (fn [[l r] & _] {:m (if (< l r) 1 0)})
         8  (fn [[l r] & _] {:m (if (= l r) 1 0)})
         9  (fn [[l] o]        (swap! o + l) nil)
         99 (constantly nil)}))


(defn read-input [path]
  (as-> path $
        (slurp $)
        (str/split $ #",")
        (map #(Long/parseLong %) $)
        (zipmap (range) $)))


(defn run-program [{:keys [i m o] :as inst}]
  (let [op      (mod (get m i) 100)
        ABC     (->> (get m i)
                     (format "%05d")
                     (reverse)
                     (drop 2))
        st      (get @steps op)
        lrt     (map #(get m (+ i %) 0) (range 1 st))
        LR      (map (fn [ch x]
                       (case ch
                         \0 (get m x 0)        ;; position mode
                         \1 x                  ;; immediate mode
                         \2 (get m (+ x @o) 0) ;; relative mode
                         ;; default
                         (throw (IllegalArgumentException.))))
                     ABC (take 2 lrt))
        t       (case (last (take (dec st) ABC))
                  \2 (+ (last lrt) @o)
                  ;; default
                  (last lrt))
        {ii :i
         dm :m} ((get @funcs op) LR o)
        dm      (when dm {t dm})]
    (if (contains? @exit-codes op)
      (merge-with + inst {:i st})
      (recur {:i (or ii (+ i st))
              :m (merge m dm)
              :o o}))))


(defn solver [inst]
  (run-program {:i 0
                :m inst}))


(defn solve [path]

  (let [input (read-input path)]

    ;; part 1 - provide 1 as an input
    (solver input)

    ;; part 2 - provide 5 as an input
    (solver input)))
