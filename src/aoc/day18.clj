(ns aoc.day18
  (:require [clojure.string :as str]))


(def ^:const center \!)

(def ^:const wall \#)
(def ^:const free \.)
(def ^:const entrance \@)
(def ^:const dkeys (set (map char (range (int \a) (inc (int \z))))))
(def ^:const doors (set (map char (range (int \A) (inc (int \Z))))))

(def lcase (comp first str/lower-case str))
(def ucase (comp first str/upper-case str))

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
       (remove (comp (partial = wall) second))
       (into {})))


(defn nil-safe-min [& args]
  (->> args
       (remove nil?)
       (apply min)))


(defn filter-extra-doors [grid]
  (let [chrs  (distinct (map second grid))
        ks    (filter dkeys chrs)
        ds    (filter doors chrs)
        extra (set (remove (set (map ucase ks)) ds))]
    (into {} (keep (fn [[k v]] (if (extra v) [k free] [k v])) grid))))


(defn find-pairwise-distances-for-start [grid start]
  (loop [cur     (list (ffirst (filter (comp (partial = start) val) grid)))
         visited {(first cur) {:steps 0 :doors []}}]

    (let [new-cur     (mapcat (fn [xy]
                                (keep (fn [delta]
                                        (let [newxy (merge-with + xy delta)]
                                          (when (and (grid newxy)
                                                     (nil? (visited newxy))) [newxy (visited xy)])))
                                      (vals dxdy)))
                              cur)

          new-visited (reduce (fn [coll [xy {drs :doors stp :steps}]]
                                (assoc coll xy {:steps (inc stp)
                                                :doors (if-let [d (doors (grid xy))]
                                                         (conj drs d)
                                                         drs)}))
                              visited
                              new-cur)
          new-cur     (map first new-cur)]

      (if (empty? new-cur)
        ;; return
        (->> visited
             (map (fn [[k v]] [k (merge v {:val (grid k)})]))
             (remove (comp (partial = free) :val second))
             (map second))
        ;; else continue
        (recur new-cur new-visited)))))


(defn find-pairwise-distances [grid]
  (let [;; bfs - find pairwise distances
        all-starts (remove (partial contains? #{free wall}) (vals grid))
        res        (->> all-starts
                        (map (partial find-pairwise-distances-for-start grid))
                        (zipmap all-starts)
                        (mapcat (fn [[k cl]]
                                  (map (fn [{v :val :as mp}]
                                         [[k v] (dissoc mp :val)]) cl)))
                        (reduce #(assoc %1 (first %2) (second %2)) {}))]
    res))


(defn bfs [edges]
  (let [vertices   (distinct (flatten (keys edges)))
        dkeys-only (set (filter dkeys vertices))]
    (loop [iii       -1
           distances {[entrance (set '())] 0}]
      (let [cur           (filter #(<= iii (count (second %))) (keys distances))
            new-distances (reduce (fn [coll [node1 visited]]
                                    (apply merge-with nil-safe-min coll
                                           (map (fn [node2]
                                                  (let [viz   (if (dkeys node1)
                                                                (conj visited node1)
                                                                visited)
                                                        {ds :doors s :steps} (edges [node1 node2])
                                                        old-s (distances [node2 viz])
                                                        new-s (+ s (distances [node1 visited]))
                                                        upd-s (if old-s (min old-s new-s) new-s)]
                                                    (when (every? viz (map lcase ds))
                                                      {[node2 viz] upd-s})))
                                                vertices)))
                                  distances
                                  cur)]

        (if (->> new-distances
                 (filter (comp (partial = dkeys-only) second first))
                 (count)
                 (= (count vertices)))
          (filter (comp (partial = dkeys-only) second first) new-distances)
          (recur (inc iii) new-distances))))))


(defn solve [path]

  ;; part 1
  (let [grid (read-input (str path "a"))
        ;; bfs - find all edges
        res  (find-pairwise-distances grid)
        ;; bfs - find min path
        ans1 (time (bfs res))]

    ;; 443 seconds
    (println "Part 1:" (apply min-key val ans1)))

  ;; For part 2, find the area on your map in the middle that looks like this:
  ;;
  ;; ...
  ;; .@.
  ;; ...
  ;;
  ;; Update your map to instead use the correct data:
  ;;
  ;; @#@
  ;; #!#
  ;; @#@

  ;; Note - I changed the center char to '!' to make locating center easier

  ;; part 2
  ;; assume no deadlock
  (let [grid  (read-input (str path "b"))
        {xc :x
         yc :y} (ffirst (filter (comp (partial = center) val) grid))

        grids (map (fn [z]
                     (let [fnx (if (even? z) < >)
                           fny (if (< 1 z) < >)]
                       (filter (fn [[{:keys [x y]} _]]
                                 (and (fnx x xc) (fny y yc))) grid)))
                   (range 4))

        grids (map filter-extra-doors grids)

        ress  (map find-pairwise-distances grids)
        ;; bfs - find min path
        ans2  (map bfs ress)
        ans2  (map (partial apply min-key val) ans2)]

    (println "Part 2:" (apply + (map second ans2)))))
