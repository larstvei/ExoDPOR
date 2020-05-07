(ns exogenous.dpor
  (:require [clojure.set :refer [union difference intersection]]
            [exogenous.relations :refer [relates? enabled-candidates]]))

(defn new-node [ev enabled]
  {:backset #{ev} :enabled enabled :blocked #{} :sleep #{}})

(defn enabled-after [trace i {:keys [mhb]}]
  (let [pre (set (subvec trace 0 i))
        post (subvec trace i)]
    (enabled-candidates post pre mhb)))

(defn update-node [ss trace i {:keys [hb] :as rels}]
  (let [pre (subvec trace 0 i)
        ev (trace i)
        node (ss pre)]
    (if-not node
      (new-node ev (enabled-after trace i rels))
      (let [new-backset (conj (:backset node) ev)
            prev-sleep (if (empty? pre) #{} (:sleep (ss (pop pre))))
            new-sleep (set (remove #(relates? hb ev %) prev-sleep))]
        (-> node
            (assoc :backset new-backset)
            (update :sleep union new-sleep))))))

(defn not-dep [trace i {hb :hb}]
  (let [ev (trace i)
        t (subvec trace (inc i))]
    (filterv #(not (relates? hb ev %)) t)))

(defn reversible-race? [trace i j {:keys [mhb interference hb]}]
  (let [ev (trace i)
        ev2 (trace j)]
    (and (not (relates? mhb ev2 ev))
         ;; Should we use hb or interference here?
         (relates? hb ev2 ev)
         (empty? (for [k (range (inc j) i)
                       :let [ev3 (trace k)]
                       :when (and (relates? hb ev2 ev3)
                                  (relates? hb ev3 ev))]
                   k)))))

(defn initial-set [pre v {hb :hb}]
  (difference (set v)
              (set (for [i (range (count v))
                         j (range i)
                         :when (relates? hb (v j) (v i))]
                     (v i)))))

(defn update-backset [search-state trace i j rels]
  (let [ev1 (trace i)
        pre (subvec trace 0 j)
        {:keys [enabled backset sleep]} (search-state pre)
        v (conj (not-dep trace j rels) ev1)
        initials (intersection enabled (initial-set pre v rels))]
    (if (empty? (intersection initials backset))
      (update-in search-state [pre :backset] conj
                 (if (initials ev1) ev1 (first initials)))
      search-state)))

(defn update-backsets [search-state trace i rels]
  (let [t (subvec trace 0 i)]
    (->> (filter (fn [j] (reversible-race? trace i j rels)) (range i))
         (reduce (fn [ss j] (update-backset ss trace i j rels)) search-state))))

(defn add-trace [search-state seed-trace trace rels]
  (-> (fn [ss i]
        (let [pre (subvec trace 0 i)
              ev (trace i)
              node (update-node ss trace i rels)]
          (-> (assoc ss pre node)
              (update-in [pre :sleep] conj ev)
              (update-backsets trace i rels))))
      (reduce search-state (range (count trace)))
      (assoc-in [trace :enabled] (enabled-after trace (count trace) rels))))

(defmulti backtrack :backtracking)

(defmethod backtrack :backsets [{:keys [search-state]}]
  (-> (fn [[seed-trace {:keys [backset sleep]}]]
        (map (partial conj seed-trace) (difference backset sleep)))
      (mapcat search-state)
      set))

(defmethod backtrack :sleep-only [{:keys [search-state]}]
  (let [candidates (mapcat (fn [[seed-trace {:keys [enabled sleep]}]]
                             (map (partial conj seed-trace)
                                  (difference enabled sleep)))
                           search-state)]
    (set candidates)))

(defmethod backtrack :naive [{:keys [search-state]}]
  (let [candidates (mapcat (fn [[seed-trace {:keys [enabled]}]]
                             (map (partial conj seed-trace) enabled))
                           search-state)]
    (set (remove search-state candidates))))
