(ns exogenous.dpor
  (:require [clojure.set :refer [union difference intersection]]))

(defn new-node [ev enabled]
  {:backset #{ev} :enabled enabled :blocked #{} :sleep #{ev}})

(defn update-node [node ev enabled]
  (merge-with union node (new-node ev enabled)))

(defn next-sleep-set [node ev sleep interference]
  (or node
      (let [independent? (comp not interference (partial vector ev))
            next-sleep (filter independent? sleep)]
        {:sleep (into #{} next-sleep)})))

(defn not-dep [t ev e interference]
  (let [independent? (comp not interference (partial vector e))]
    (conj (vec (take-while independent? t)) ev)))

(defn update-backsets [s trace ev interference]
  (-> (fn [s i]
        (let [e (trace i)
              pre (subvec trace 0 i)
              v (not-dep (subvec trace (inc i)) ev e interference)
              n (s pre)
              ;; This is not correct
              initial (into #{} (filter #((:enabled n) %) v))]
          (if (and (not-empty initial)
                   (interference [e ev])
                   (empty? (intersection initial (:backset n))))
            (update-in s [pre :backset] conj (first initial))
            s)))
      (reduce s (range (count trace)))))

(defn add-trace [search-state trace enabled interference]
  (-> (fn [s i]
        (let [t1 (subvec trace 0 i)
              t2 (subvec trace 0 (inc i))
              ev (trace i)
              node (update-node (s t1) ev (enabled i))
              next-node (next-sleep-set (s t2) ev (:sleep node) interference)]
          (-> (assoc s t1 node)
              (assoc t2 next-node)
              (update-backsets t1 ev interference))))
      (reduce search-state (range (count trace)))
      (update trace (partial merge-with union) {:enabled (last enabled)})))

(defn backtrack [search-state]
  (-> (fn [[trace {:keys [:backset :sleep]}]]
        (map (partial conj trace) (difference backset sleep)))
      (mapcat search-state)))

(defn backtrack-naive [search-state]
  (-> (fn [[trace {:keys [:enabled :sleep]}]]
        (map (partial conj trace) (difference enabled sleep)))
      (mapcat search-state)))

#_(def backtrack backtrack-naive)
