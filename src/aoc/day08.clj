(ns aoc.day08
  (:require [clojure.string :as str]))


(def ^:const width 25)
(def ^:const height 6)


(defn read-input [path]
  (slurp path))


(defn solve [path]

  (let [layer (->> (read-input path)
                   (partition (* height width))
                   (map frequencies)
                   (apply min-key #(get % \0)))
        res   (* (get layer \1) (get layer \2))]
    (println res))


  (let [chrs  {\0 " "
               \1 "*"}
        layer (read-input path)
        cnt   (long (/ (count layer) width height))
        res   (->> layer
                   (partition (* height width))
                   (apply interleave)
                   (partition cnt)
                   (map (partial remove (partial = \2)))
                   (map first)
                   (map chrs)
                   (partition width)
                   (map (partial str/join ""))
                   (str/join "\n"))]
    (println res)))
