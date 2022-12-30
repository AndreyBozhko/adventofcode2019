(ns aoc.day14
  (:require [clojure.string :as str]
            [aoc.tools :refer [topo-sort]]))


(defn read-input [path]
  (->> path
       (slurp)
       (str/split-lines)
       (map (fn [reaction]
              (let [f   (fn [ing]
                          (let [[c n] (str/split ing #" ")
                                c (Long/parseLong c)]
                            {n c}))
                    [src trg] (str/split reaction #" => ")
                    src (str/split src #", ")
                    src (map f src)
                    trg (f trg)]
                [(first (keys trg)) {:amt (first (vals trg))
                                     :ing (into {} src)}])))
       (into {})))


(defn calculate [{:keys [quantities reactions rounding]}]
  (let [;; get edges of a DAG
        edges (reduce (fn [coll [trg {src :ing}]]
                        (assoc coll trg (set (keys src))))
                      {}
                      reactions)
        ;; perform topological sort of vertices
        ings  (topo-sort (merge edges {"ORE" #{}}))
        ;; calculate necessary quantities, going through vertices in order
        newq  (reduce (fn [coll trg]
                        (let [{:keys [amt ing]
                               :or   {amt 1}} (get reactions trg)
                              cur    (get coll trg)
                              ;; cur - actual amount of target ingredient calculated so far
                              ;; amt - amount of target ingredient that can be produced in reaction
                              factor (rounding (/ cur amt))
                              ;; add - amounts of source ingredients needed to produce cur amount of target ingredient
                              add    (->> ing
                                          (map (fn [[k v]]
                                                 [k (* factor v)]))
                                          (into {}))]

                          (if (seq ing)
                            (merge-with + coll add)
                            coll)))
                      quantities
                      ings)]
    (get newq "ORE")))


(defn solve [path]

  (let [input (read-input path)]

    ;; part 1
    (->> {:quantities {"FUEL" 1}
          :rounding   #(long (Math/ceil %))
          :reactions  input}
         (calculate)
         (format "Need %d ORE to produce 1 FUEL")
         (println))

    ;; part 2
    (->> {:quantities {"FUEL" 1}
          :rounding   double
          :reactions  input}
         (calculate)
         (/ (Math/pow 10 12))
         (long)
         (format "Can produce %d FUEL from 1.0E+12 ORE")
         (println))))
