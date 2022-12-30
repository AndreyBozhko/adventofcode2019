(ns aoc.day03
  (:require [clojure.string :as str]))

(defn get-edges [inst]
  (reduce (fn [coll x]
            (let [{:keys [x2 y2 st]} (first coll)
                  x1                 (or x2 0)
                  y1                 (or y2 0)
                  st                 (or st 0)
                  d                  (first x)
                  l                  (Long/parseLong (str/join "" (next x)))
                  dxdy               (case d
                                       \U {:x 0 :y l}
                                       \D {:x 0 :y (- l)}
                                       \L {:x (- l) :y 0}
                                       \R {:x l :y 0})]
              (conj coll
                    {:st (+ st l) ;; steps to (x2,y2)
                     :x1 x1
                     :y1 y1
                     :x2 (+ x1 (:x dxdy))
                     :y2 (+ y1 (:y dxdy))})))
          '()
          inst))


(defn read-input [path]
  (->> path
       (slurp)
       (str/split-lines)
       (map #(str/split % #","))
       (map get-edges)
       (zipmap [:w1 :w2])))


(defn horizontal? [{:keys [y1 y2]}] (= y1 y2))
(defn vertical? [{:keys [x1 x2]}] (= x1 x2))

(defn get-intersection [e1 e2]
  (let [he (if (horizontal? e1) e1 e2)
        ve (if (horizontal? e1) e2 e1)]
    (when (and (horizontal? he)
               (vertical? ve)
               ;; check if they intersect
               (< (min (:y1 ve) (:y2 ve))
                  (:y1 he)
                  (max (:y1 ve) (:y2 ve)))
               (< (min (:x1 he) (:x2 he))
                  (:x1 ve)
                  (max (:x1 he) (:x2 he))))
      ;; return both taxi and step distances
      {:taxi (+ (Math/abs (:x1 ve))
                (Math/abs (:y1 he)))
       :step (+ (- (:st he) (Math/abs (- (:y2 ve) (:y2 he))))
                (- (:st ve) (Math/abs (- (:x2 ve) (:x2 he)))))})))


(defn calculate [dist-fn {:keys [w1 w2]}]
  (let [res (atom #{})]
    ;; construct set of distances to intersections
    (doseq [e1    w1
            e2    w2
            :let  [r (dist-fn (get-intersection e1 e2))]
            :when r]
      (swap! res conj r))
    ;; find minimum
    (apply min (disj @res 0))))


(defn solve [path]

  (let [input (read-input path)
        ans-1 (calculate :taxi input)
        ans-2 (calculate :step input)]

    (println ans-1)
    (println ans-2)))
