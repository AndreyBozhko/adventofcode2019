(ns aoc.day16)


(def ^:const m-length 8)
(def ^:const o-length 7)
(def ^:const phases 100)
(def ^:const factor 10000)
(def ^:const base-pattern '(0 1 0 -1))


(defn read-input [path]
  (->> path
       (slurp)
       (map #(Long/parseLong (str %)))))


(defn bf-calculate [steps input]
  (let [transform (fn [coll & _]
                    (->> (range 1 (inc (count coll)))
                         (map (fn [idx]
                                (->> base-pattern
                                     (mapcat (partial repeat idx))
                                     (cycle)
                                     (next)
                                     (map * coll)
                                     (apply +))))
                         (map #(Math/abs (rem % 10)))))]
    (reduce transform input (range steps))))


(defn calculate [steps reduced-input]
  (->> reduced-input
       (reduce (fn [coll x]
                 (let [old-c (next (first coll))
                       new-c (reduce #(conj %1 (rem (+ %2 (first %1)) 10))
                                     (list x)
                                     old-c)]
                   (take m-length (conj coll (reverse new-c)))))
               (list (repeat (inc steps) 0)))
       (map last)))


(defn solve [path]

  (let [input  (read-input path)
        length (count input)
        offset (->> input
                    (take o-length)
                    (apply str)
                    (#(Long/parseLong %)))]

    ; part 1 - brute-force approach
    (->> input
         (bf-calculate phases)
         (take m-length)
         (apply str)
         (println "Part 1:"))

    ;; part 2 - under condition that offset >= length(input) / 2
    (assert (<= (* length factor 1/2) offset (- (* length factor) m-length)))
    (->> input
         (reverse)
         (cycle)
         (take (- (* factor (count input)) offset))
         (calculate phases)
         (apply str)
         (println "Part 2:"))))
