(ns aoc.day23
  (:require [aoc.day05 :refer [exit-codes
                               read-input
                               run-program
                               program-reads-input
                               program-prints-output]]))


(swap! exit-codes conj 4)
(def ^:const num-computers 50)
(def ^:const NAT 255)


(defn run-program-w-io [state-w-io computer]
  (let [{:keys [inp-stack out-stack state]} (@state-w-io computer)
        cntr      (atom -1)

        new-state (loop [st state]
                    (let [tmp (with-redefs [;; override program input - input will be read from inp-stack instead of waiting for user input
                                            program-reads-input   (fn []
                                                                    (when (pos? @cntr) (swap! exit-codes conj 3))
                                                                    (or (ffirst (swap-vals! inp-stack rest))
                                                                        (do (swap! cntr inc)
                                                                            -1)))
                                            ;; override program output - output will be added to out-stack instead of printing
                                            program-prints-output (fn [x] (swap! out-stack conj x) nil)]
                                (run-program st))]

                      (if (pos? @cntr)
                        (do (swap! exit-codes disj 3)
                            tmp)
                        (recur tmp))))]

    (swap! state-w-io assoc computer {:inp-stack inp-stack
                                      :out-stack out-stack
                                      :state     new-state})))


(defn communicate [NIC]
  (let [outputs     (->> @NIC
                         (map (fn [[k v]] [k @(:out-stack v)]))
                         (remove (comp empty? second))
                         (into {}))
        new-inputs  (reduce (fn [coll j]
                              (assoc coll j {:inp-stack (atom '())}))
                            {}
                            (conj (range num-computers) NAT))
        new-outputs (reduce (fn [coll j]
                              (assoc coll j {:out-stack (atom '())}))
                            {}
                            (conj (range num-computers) NAT))]

    (doseq [i (sort > (remove (partial = NAT) (keys outputs)))]

      (loop [out (outputs i)]

        (let [[Y X c] (take 3 out)]

          (when c
            (swap! (get-in new-inputs [c :inp-stack]) conj Y)
            (swap! (get-in new-inputs [c :inp-stack]) conj X)
            (recur (drop 3 out))))))

    (swap! NIC (partial merge-with merge) new-inputs)
    (swap! NIC (partial merge-with merge) new-outputs)))


(defn solve [path]

  ;; part 1
  (let [input (read-input path)
        NIC   (atom (reduce (fn [coll i]
                              (assoc coll i {:state     {:i 0
                                                         :o (atom 0)
                                                         :m input}
                                             :inp-stack (atom (if (= i NAT) '() (list i)))
                                             :out-stack (atom '())}))
                            {}
                            (conj (range num-computers) NAT)))]

    (loop [iteration 0]

      (doseq [i (range num-computers)]
        (run-program-w-io NIC i))

      (communicate NIC)

      (if-let [XY (not-empty @(get-in @NIC [NAT :inp-stack]))]
        (println "Part 1:" (second XY))
        (recur (inc iteration)))))


  ;; part 2
  (let [input (read-input path)
        NIC   (atom (reduce (fn [coll i]
                              (assoc coll i {:state     {:i 0
                                                         :o (atom 0)
                                                         :m input}
                                             :inp-stack (atom (if (= i NAT) '() (list i)))
                                             :out-stack (atom '())}))
                            {}
                            (conj (range num-computers) NAT)))
        found (atom nil)
        seen  (atom #{})]

    (loop [iteration 0]

      (doseq [i (range num-computers)]
        (run-program-w-io NIC i))

      (communicate NIC)

      (when (empty? (->> (dissoc @NIC NAT)
                         (map (fn [[k v]] [k @(:inp-stack v)]))
                         (remove (comp empty? second))
                         (into {})))
        (let [[YY XX] (take 2 (reverse @(get-in @NIC [NAT :inp-stack])))]

          (swap! NIC (partial merge-with merge) {0 {:inp-stack (atom (list XX YY))}})
          (reset! (get-in @NIC [NAT :inp-stack]) '())
          (if (contains? @seen YY)
            (reset! found YY)
            (swap! seen conj YY))))

      (if @found
        (println "Part 2:" @found)
        (recur (inc iteration))))))
