(ns aoc.day20
  (:require [clojure.string :as str]))


(def ^:const void (first " "))
(def ^:const wall \#)
(def ^:const free \.)
(def ^:const entrance "AA")
(def ^:const exit "ZZ")

(def ^:const portal-chars (set (map char (range (int \A) (inc (int \Z))))))

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
                                     [{:x x :y y} c])
                                   line)))
       (mapcat identity)
       (remove (comp (partial contains? #{wall void}) second))
       (into {})))


(defn find-portals [grid]
  (->> grid
       (mapcat (fn [[xy v]]
                 (when (= free v)
                   (map (fn [delta]
                          (let [c2 (merge-with - xy delta)
                                c1 (merge-with - c2 delta)
                                cc (map grid [c1 c2])]
                            (when (every? portal-chars cc)
                              [(str/join "" cc) xy])))
                        (vals dxdy)))))
       (remove nil?)
       (group-by first)
       (map (fn [[k v]] [k (map second v)]))
       (into {})))


(defn get-from-start-to-end [grid update-depth portals start end]
  (let [portal-names (set (keys portals))]
    (loop [cur     (list start)
           visited {(first cur) 0}]

      (let [new-cur     (mapcat (fn [{depth :depth :as xyd}]
                                  (keep (fn [delta]
                                          (let [xy       (dissoc xyd :depth)
                                                newxy1   (merge-with + xy delta)
                                                newxy2   (merge-with + newxy1 delta)
                                                door1    (str/join "" (map grid [newxy1 newxy2]))
                                                door2    (str/join "" (map grid [newxy2 newxy1]))
                                                newxy    (if (some portal-names [door1 door2])
                                                           (->> [door1 door2]
                                                                (mapcat portals)
                                                                (remove (partial = xy))
                                                                (first))
                                                           newxy1)
                                                newdepth (if (some portal-names [door1 door2])
                                                           (update-depth depth newxy2)
                                                           depth)
                                                newxyd   (merge newxy {:depth newdepth})]
                                            (when (and (= free (grid newxy))
                                                       (<= 0 newdepth)
                                                       (nil? (visited newxyd)))
                                              [newxyd (visited xyd)])))
                                        (vals dxdy)))
                                cur)

            new-visited (reduce (fn [coll [xyd step]]
                                  (assoc coll xyd (inc step)))
                                visited
                                new-cur)
            new-cur     (map first new-cur)]

        (if (or (new-visited end)
                (empty? new-cur))
          ;; return
          (new-visited end)
          ;; else continue
          (recur new-cur new-visited))))))



(defn solve [path]

  (let [;; parse grid
        grid (read-input path)
        [xmin xmax] (apply (juxt min max) (map :x (keys grid)))
        [ymin ymax] (apply (juxt min max) (map :y (keys grid)))

        ;; locate portals
        prt  (find-portals grid)
        [start end] (map (comp first prt) [entrance exit])
        prt  (dissoc prt "AA" "ZZ")

        ;; part 1
        res1 (get-from-start-to-end grid
                                    (fn [depth _] depth)
                                    prt
                                    (merge start {:depth 0})
                                    (merge end {:depth 0}))

        ;; part 2
        res2 (get-from-start-to-end grid
                                    (fn [depth {:keys [x y]}]
                                      (if (or (contains? #{xmin xmax} x)
                                              (contains? #{ymin ymax} y))
                                        (dec depth)
                                        (inc depth)))
                                    prt
                                    (merge start {:depth 0})
                                    (merge end {:depth 0}))]

    (println "Part 1:" res1)
    (println "Part 2:" res2)))
