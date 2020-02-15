(ns exogenous.dpor
  (:require [clojure.set :refer [union difference intersection]]))

(defn new-node [ev enabled]
  {:backset #{ev} :enabled enabled :blocked #{} :sleep #{}})

(defn update-node [node ev enabled]
  (if (empty? (:backset node))
    (merge-with union node (new-node ev enabled))
    node))

(defn next-sleep-set [node ev sleep mhb interference]
  (or node
      (->> (filter #(not (or (mhb [ev %]) (interference [ev %]))) sleep)
           (into #{})
           (assoc node :sleep))))

(defn not-dep [trace i mhb interference]
  (let [ev (trace i)
        t (subvec trace (inc i))]
    (filterv #(not (or (mhb [ev %]) (interference [ev %]))) t)))

(defn reversible-race? [trace n j mhb interference]
  (let [ev (trace n)
        ev2 (trace j)]
    (and (not (mhb [ev2 ev]))
         (interference [ev2 ev])
         (empty? (for [k (range j (inc n))
                       :let [ev3 (trace k)]
                       :when (and (or (mhb [ev2 ev3]) (interference [ev2 ev3]))
                                  (or (mhb [ev3 ev]) (interference [ev3 ev])))]
                   k)))))

(defn initial-set [pre v mhb]
  (difference (set v)
              (set (for [i (range (count v))
                         j (range i)
                         :when (mhb [(v j) (v i)])]
                     (v i)))))

(defn update-backset [search-state trace n j enabled mhb interference]
  (let [ev1 (trace n)
        pre (subvec trace 0 j)
        v (conj (not-dep trace j mhb interference) ev1)
        initials (intersection enabled (initial-set pre v mhb))
        backset (:backset (search-state pre))]
    (if (empty? (intersection initials backset))
      (update-in search-state [pre :backset] conj (first initials))
      search-state)))

(defn update-backsets [search-state trace n enabled-sets mhb interference]
  (let [t (subvec trace 0 n)]
    (-> (fn [s j]
          (if (reversible-race? trace n j mhb interference)
            (update-backset s trace n j (enabled-sets j) mhb interference)
            s))
        (reduce search-state (range n)))))

(defn add-trace [search-state trace enabled-sets mhb interference]
  (-> (fn [s i]
        (let [t1 (subvec trace 0 i)
              t2 (subvec trace 0 (inc i))
              ev (trace i)
              enabled (enabled-sets i)
              node (update-node (s t1) ev enabled)
              next-node (next-sleep-set (s t2) ev (:sleep node) mhb interference)]
          (-> (assoc s t1 node)
              (update-backsets trace i enabled-sets mhb interference)
              (assoc t2 next-node)
              (update-in [t1 :sleep] conj ev))))
      (reduce search-state (range (count trace)))
      (assoc-in [trace :enabled] (last enabled-sets))))

(defn backtrack [search-state]
  (-> (fn [[trace {:keys [:backset :sleep]}]]
        (map (partial conj trace) (difference backset sleep)))
      (mapcat search-state)))

(defn backtrack-depth-first [search-state trace]
  (let [backsets (-> (fn [i e]
                       (let [t (subvec trace 0 i)
                             node (search-state t)]
                         (map (partial conj t) (difference (:backset node) (:sleep node)))))
                     (map-indexed trace))]
    (last (filter not-empty backsets))))

(defn backtrack-naive [search-state]
  (-> (fn [[trace {:keys [:enabled :sleep]}]]
        (map (partial conj trace) (difference enabled sleep)))
      (mapcat search-state)))

#_(def backtrack backtrack-naive)
