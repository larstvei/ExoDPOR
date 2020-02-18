(ns exogenous.dpor
  (:require [clojure.set :refer [union difference intersection]]
            [exogenous.relations :refer [relates?]]))

(defn new-node [ev enabled]
  {:backset #{ev} :enabled enabled :blocked #{} :sleep #{}})

(defn update-node [node ev enabled]
  (if (empty? (:backset node))
    (merge-with union node (new-node ev enabled))
    node))

(defn next-sleep-set [node ev sleep {hb :hb}]
  (or node
      (->> (filter #(not (relates? hb ev %)) sleep)
           (into #{})
           (assoc node :sleep))))

(defn not-dep [trace i {hb :hb}]
  (let [ev (trace i)
        t (subvec trace (inc i))]
    (filterv #(not (relates? hb ev %)) t)))

(defn reversible-race? [trace n j {:keys [:mhb :interference :hb]}]
  (let [ev (trace n)
        ev2 (trace j)]
    (and (not (relates? mhb ev2 ev))
         (relates? interference ev2 ev)
         (empty? (for [k (range j (inc n))
                       :let [ev3 (trace k)]
                       :when (and (relates? hb ev2 ev3)
                                  (relates? hb ev3 ev))]
                   k)))))

(defn initial-set [pre v {mhb :mhb}]
  (difference (set v)
              (set (for [i (range (count v))
                         j (range i)
                         :when (relates? mhb (v j) (v i))]
                     (v i)))))

(defn update-backset [search-state trace n j rels]
  (let [ev1 (trace n)
        pre (subvec trace 0 j)
        enabled (:enabled (search-state pre))
        v (conj (not-dep trace j rels) ev1)
        initials (intersection enabled (initial-set pre v rels))
        backset (:backset (search-state pre))]
    (if (empty? (intersection initials backset))
      (update-in search-state [pre :backset] conj (first initials))
      search-state)))

(defn update-backsets [search-state trace n rels]
  (let [t (subvec trace 0 n)]
    (-> (fn [s j]
          (if (reversible-race? trace n j rels)
            (update-backset s trace n j rels)
            s))
        (reduce search-state (range n)))))

(defn enabled-after [trace i {mhb :mhb}]
  (let [pre (set (subvec trace 0 i))
        post (subvec trace i)]
    (->> (filter (fn [ev] (empty? (difference (mhb ev) pre))) post)
         (into #{}))))

(defn add-trace [search-state trace rels]
  (-> (fn [s i]
        (let [t1 (subvec trace 0 i)
              t2 (subvec trace 0 (inc i))
              ev (trace i)
              enabled (enabled-after trace i rels)
              node (update-node (s t1) ev enabled)
              next-node (next-sleep-set (s t2) ev (:sleep node) rels)]
          (-> (assoc s t1 node)
              (update-backsets trace i rels)
              (assoc t2 next-node)
              (update-in [t1 :sleep] conj ev))))
      (reduce search-state (range (count trace)))
      (assoc-in [trace :enabled] (enabled-after trace (count trace) rels))))

(defn backtrack [search-state]
  (-> (fn [[trace {:keys [:backset :sleep]}]]
        (map (partial conj trace) (difference backset sleep)))
      (mapcat search-state)))

(defn backtrack-depth-first [search-state trace]
  (let [backsets (-> (fn [i e]
                       (let [t (subvec trace 0 i)
                             node (search-state t)
                             backtrack (difference (:backset node) (:sleep node))]
                         (map (partial conj t) backtrack)))
                     (map-indexed trace))]
    (last (filter not-empty backsets))))

(defn backtrack-naive [search-state]
  (-> (fn [[trace {:keys [:enabled :sleep]}]]
        (map (partial conj trace) (difference enabled sleep)))
      (mapcat search-state)))

#_(def backtrack backtrack-naive)
