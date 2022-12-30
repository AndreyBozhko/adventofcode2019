(ns aoc.day24
  (:require [clojure.string :as str]))


(def ^:const size 5)
(def ^:const center (long (/ size 2)))
(def ^:const minutes 200)


(def dxdy
  {1 {:x 0 :y 1}
   2 {:x 0 :y -1}
   3 {:x -1 :y 0}
   4 {:x 1 :y 0}})


(defn read-input [path]
  (->> path
       (slurp)
       (str/split-lines)
       (map-indexed (fn [y line]
                      (map-indexed (fn [x c]
                                     [{:x x :y y} (if (= \# c) 1 0)])
                                   line)))
       (mapcat identity)
       (into {})))


(defn basic-cnt [grids level xy]
  (->> (vals dxdy)
       (map (partial merge-with + xy))
       (map #(get (grids level) % 0))
       (apply +)))


(defn recursive-cnt [grids level {:keys [x y] :as xy}]
  (let [base-cnt     (basic-cnt grids level xy)

        dec-level-fn #(get (grids (dec level)) % 0)

        inc-level-fn #(apply + (map second
                                    (filter (fn [[xy _]]
                                              (= (xy (first %)) (second %)))
                                            (grids (inc level)))))

        add-cnt1     (cond
                       (zero? x) (dec-level-fn {:x (dec center) :y center})
                       (= (dec size) x) (dec-level-fn {:x (inc center) :y center})
                       :else 0)

        add-cnt2     (cond
                       (zero? y) (dec-level-fn {:x center :y (dec center)})
                       (= (dec size) y) (dec-level-fn {:x center :y (inc center)})
                       :else 0)

        add-cnt3     (cond
                       (= {:x center :y (dec center)} xy) (inc-level-fn [:y 0])
                       (= {:x center :y (inc center)} xy) (inc-level-fn [:y (dec size)])
                       (= {:x (dec center) :y center} xy) (inc-level-fn [:x 0])
                       (= {:x (inc center) :y center} xy) (inc-level-fn [:x (dec size)])
                       :else 0)]

    (+ base-cnt add-cnt1 add-cnt2 add-cnt3)))


(defn update-grid [{:keys [grids level count-fn exclude-xy]}]
  (reduce (fn [coll [xy bug]]
            (let [cnt    (count-fn grids level xy)
                  newbug (cond
                           (and (= 1 bug) (not= 1 cnt)) 0
                           (and (= 0 bug) (contains? #{1 2} cnt)) 1
                           :else bug)]
              (assoc coll xy newbug)))
          {}
          (remove #(contains? exclude-xy (first %))
                  (grids level))))


(defn get-rating [grid]
  (->> grid
       (map (fn [[{:keys [x y]} bug]]
              (* bug (Math/pow 2 (+ x (* size y))))))
       (apply +)
       (long)))


(defn evolve-basic-grid [input]
  (loop [seen  #{}
         grids {0 input}]
    (let [new-grids (assoc grids 0 (update-grid {:grids      grids
                                                 :level      0
                                                 :count-fn   basic-cnt
                                                 :exclude-xy #{}}))
          rating    (get-rating (grids 0))]
      (if (contains? seen rating)
        rating
        (recur (conj seen rating) new-grids)))))


(defn evolve-recursive-grids [input]
  (let [zero-input (->> input
                        (map (fn [[xy _]] [xy 0]))
                        (into {}))
        grids      {0 input}]

    (loop [iteration 0
           old-grids grids]

      (let [[l r] (apply (juxt min max) (keys old-grids))
            [l r] [(dec l) (inc r)]

            new-grids (reduce (fn [coll level]
                                (assoc coll
                                  level (update-grid {:grids      (merge old-grids
                                                                         {l zero-input
                                                                          r zero-input})
                                                      :level      level
                                                      :count-fn   recursive-cnt
                                                      :exclude-xy #{{:x center :y center}}})))
                              {}
                              (conj (keys old-grids) l r))

            new-grids (reduce (fn [coll q]
                                (if (zero? (apply + (vals (coll q))))
                                  (dissoc coll q)
                                  coll))
                              new-grids
                              [l r])]

        (if (= minutes iteration)
          (apply + (mapcat (comp vals second) old-grids))
          (recur (inc iteration) new-grids))))))


(defn solve [path]

  (let [inp  (read-input path)

        ;; part 1
        ans1 (evolve-basic-grid inp)

        ;; part 2
        ans2 (evolve-recursive-grids inp)]

    (println "Part 1:" ans1)
    (println "Part 2:" ans2)))
