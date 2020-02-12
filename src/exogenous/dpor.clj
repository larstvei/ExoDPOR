(ns exogenous.dpor
  (:require [clojure.set :refer [union difference intersection]]))

(defn new-node [ev enabled]
  {:backset #{ev} :enabled enabled :blocked #{} :sleep #{}})

(defn update-node [node ev enabled]
  (merge-with union node (new-node ev enabled)))

(defn next-sleep-set [node ev sleep interference]
  (->> (filter #(not (interference [ev %])) sleep)
       (into #{})
       (update (or node {}) :sleep (fnil union #{}))))

(defn not-dep [trace i mhb interference]
  (let [ev (trace i)
        t (subvec trace (inc i))]
    (filterv #(not (or (mhb [ev %]) (interference [ev %]))) t)))

(defn race? [trace n j mhb interference]
  (let [ev (trace n)
        ev2 (trace j)]
    (and (interference [ev2 ev])
         (empty? (for [k (range n (inc j))
                       :let [ev3 (trace k)]
                       :when (and (or (mhb [ev2 ev3]) (interference [ev2 ev3]))
                                  (or (mhb [ev3 ev]) (interference [ev3 ev])))]
                   k)))))

(defn reversible-events? [search-state trace n j mhb]
  (not (mhb [(trace j) (trace n)])))

(defn initial-set [pre v mhb]
  (difference (set v)
              (set (for [ev1 v ev2 v
                         :when (mhb [ev1 ev2])]
                     ev2))))

(defn update-backset [search-state trace n j mhb interference]
  (let [ev1 (trace n)
        pre (subvec trace 0 j)
        v (conj (not-dep trace j mhb interference) ev1)
        initials (initial-set pre v mhb)
        backset (:backset (search-state pre))]
    (if (empty? (intersection initials backset))
      (update-in search-state [pre :backset] conj (first initials))
      search-state)))

(defn update-backsets [search-state trace n mhb interference]
  (let [t (subvec trace 0 n)]
    (-> (fn [s j]
          (if (and (race? trace n j mhb interference)
                   (reversible-events? s trace n j mhb))
            (update-backset s trace n j mhb interference)
            s))
        (reduce search-state (range n)))))

(defn add-trace [search-state trace enabled-sets mhb interference]
  (-> (fn [s i]
        (let [t1 (subvec trace 0 i)
              t2 (subvec trace 0 (inc i))
              ev (trace i)
              enabled (enabled-sets i)
              node (update-node (s t1) ev enabled)
              next-node (next-sleep-set (s t2) ev (:sleep node) interference)]
          (-> (assoc s t1 node)
              (assoc t2 next-node)
              (update-backsets trace i mhb interference)
              (update-in [t1 :sleep] conj ev))))
      (reduce search-state (range (count trace)))
      (update trace (partial merge-with union)
              {:enabled (last enabled-sets)})))

(defn backtrack [search-state]
  (-> (fn [[trace {:keys [:backset :sleep]}]]
        (map (partial conj trace) (difference backset sleep)))
      (mapcat search-state)))

(defn backtrack-naive [search-state]
  (-> (fn [[trace {:keys [:enabled :sleep]}]]
        (map (partial conj trace) (difference enabled sleep)))
      (mapcat search-state)))

#_(def backtrack backtrack-naive)
