(ns aoc.day12
  (:require [clojure.string :as str]))


(defn read-input [path]
  (->> path
       (slurp)
       (str/split-lines)
       (map (fn [coll]
              (->> coll
                   (re-seq #"=(-?\d+)")
                   (map second)
                   (map #(Long/parseLong %))
                   (zipmap [:x :y :z]))))))


(defn total-energy [{:keys [r v]}]
  (let [f (fn [x]
            (->> (vals x)
                 (map #(Math/abs %))
                 (apply +)))
        p (map f r)
        k (map f v)]
    (apply + (map * p k))))


(defn lcm
  ([a b]
   (->> (range)
        (next)
        (map (partial * a))
        (keep #(when (zero? (mod % b)) %))
        (first)))
  ([a b & more]
   (if-not (seq more)
     (lcm a b)
     (apply lcm (lcm a b) more))))


(defn simulate [steps input]
  (loop [j 1
         m (atom {})
         r input
         v (repeat (count input)
                   (zipmap [:x :y :z] (repeat 0)))]

    (let [new-v (->> v
                     (interleave r)
                     (partition 2)
                     (map (fn [[rr vv]]
                            (reduce (partial merge-with -)
                                    vv
                                    (map (partial merge-with compare rr) r)))))
          new-r (map (partial merge-with +) r new-v)]

      (doseq [xyz [:x :y :z]]
        (when (and (nil? (get @m xyz))
                   (= (map xyz new-r) (map xyz input))
                   (every? zero? (map xyz new-v)))
          (swap! m assoc xyz j)))

      ;; Answer to part 1
      (when (= steps j)
        (println "Total energy:" (total-energy {:r new-r :v new-v})))

      ;; Answer to part 2
      (when (= 3 (count (keys @m)))
        (println "Number of steps:" (apply lcm (vals @m))))

      ;; continue until both answers are obtained
      (when (or (< j steps)
                (> 3 (count (keys @m))))
        (recur (inc j) m new-r new-v)))))


(defn solve [path]
  (let [input (read-input path)]

    (simulate 1000 input)))
