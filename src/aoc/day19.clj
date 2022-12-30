(ns aoc.day19
  (:require [aoc.tools :refer [cartesian-product]]
            [aoc.day05 :refer [read-input
                               run-program
                               program-reads-input
                               program-prints-output]]))


(def ^:const xmax 50)
(def ^:const ymax 50)
(def ^:const size 100)
(def ^:const limit 10000)


(defn check-xy
  "Run program defined by state 'st' for a given input"
  [st {:keys [x y]}]
  (let [stack (atom '())]
    (with-redefs [;; override program input - input will be read from stack instead of waiting for user input
                  program-reads-input   (fn [] (ffirst (swap-vals! stack next)))
                  ;; override program output - output will be added to stack instead of printing
                  program-prints-output (fn [x] (swap! stack conj x) nil)]
      (program-prints-output y)
      (program-prints-output x)
      (run-program st)
      (program-reads-input))))


(defn get-grid [st]
  (reduce (fn [coll [x y]]
            (let [xy {:x x :y y}]
              (assoc coll xy (check-xy st xy))))
          {}
          (cartesian-product (range xmax) (range ymax))))


(defn search-fn [st x]
  (let [beam? (fn [xy] (= 1 (check-xy st xy)))
        void? (complement beam?)

        y     (loop [yy x]
                (if (beam? {:x x :y yy})
                  yy
                  (recur (inc yy))))
        xy    {:x x :y y}

        cmp   (cond
                (void? {:x (- x (dec size)) :y (+ y (dec size))}) :right
                (beam? {:x (- x size) :y (+ y size)}) :left
                :else :stop)]

    (merge xy {:cmp cmp})))


(defn binary-search-over-x [st left-x right-x]
  (loop [left  left-x
         right right-x]

    (let [mid (long (/ (+ left right) 2))
          {:keys [cmp x y]} (search-fn st mid)]

      (case cmp
        :stop {:x x :y y}
        :left (recur left mid)
        :right (recur (inc mid) right)))))

(defn solve [path]

  (let [st    {:i 0
               :o (atom 0)
               :m (read-input path)}

        ;; part 1
        ;; run program
        grid  (get-grid st)
        grid  (into {} (filter (comp (partial = 1) val) grid))
        ans1  (count (vals grid))

        ;; part 2
        ;; binary search
        {:keys [x]} (binary-search-over-x st xmax limit)

        xtra  (range (- x xmax) (inc x))                    ;; choice to such range is arbitrary

        {:keys [x y]} (->> xtra
                           (map (partial search-fn st))
                           (remove (comp (partial = :right) :cmp))
                           (apply min-key :x))

        ans2  (+ y (* 10000 (- x (dec size))))]

    (println "Part 1:" ans1)
    (println "Part 2:" ans2)))
