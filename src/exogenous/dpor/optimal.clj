(ns exogenous.dpor.optimal
  (:require [clojure.set :as set]
            [exogenous.dpor.common :refer :all]
            [exogenous.dpor.wakeup :as wut]))

(defn update-backtracking
  "Given a `search-state`, a `trace`, with two positions `i` and `j` that
  corresponds to events that are in a reversible race, and relations `rels`,
  return an updated `search-state` that guarantees that a trace in which the
  race is reversed is eventually executed.

  If there is no weak initial for the non-dependent events after `i` in the
  sleep set before `i`, then add an extension that is guaranteed to reverse the
  race."
  [search-state i j {:keys [trace rels]}]
  (let [pre (subvec trace 0 i)
        ev2 (trace j)
        {:keys [enabled sleep]} (search-state pre)
        v (conj (not-dep trace i rels) ev2)
        weak-initials (weak-initial-set pre v enabled rels)]
    (if (empty? (set/intersection weak-initials sleep))
      (update-in search-state [pre :wut] wut/insert pre v rels)
      search-state)))

(defn backtrack-disabled [search-state {:keys [trace rels] :as args}]
  (-> (fn [ss [i j disabled-event]]
        (let [new-args (assoc-in args [:trace j] disabled-event)]
          (println i j disabled-event)
          (println (:trace new-args))
          (update-backtracking ss i j new-args)))
      (reduce search-state (disabled-races search-state trace rels))))

(defn backtrack-races
  "Given a `search-state`, a `trace`, and relations `rels`, return an updated
  `search-state` where each reversible race will eventually be explored from
  some extension."
  [search-state {:keys [trace rels] :as args}]
  (->> (reversible-races search-state trace rels)
       (reduce (fn [ss [i j]] (update-backtracking ss i j args)) search-state)))

(defn initialize-new-nodes
  "Given a `search-state` and arguments `trace`, `enabled-disabled` and relations
  `rels`, return an updated `search-state` where new nodes in the trace are
  initialized.

  Note that we walk the entire trace, even though it would be sufficient to
  start at the longest prefix of `trace` that already is associated with a node
  (i.e. the point from which we extended the trace). Any node already in the
  tree is left unmodified."
  ([search-state args]
   (initialize-new-nodes search-state 0 #{} wut/empty-wut args))
  ([ss i sleep wut {:keys [trace enabled-disabled rels] :as args}]
   (let [{:keys [enabled disabled]} (enabled-disabled i)
         pre (subvec trace 0 i)
         node (ss pre)]
     (cond (= i (count trace))
           (-> ss
               (assoc-in [trace :wut] wut/empty-wut)
               (assoc-in [trace :sleep] #{})
               (assoc-in [trace :enabled] enabled)
               (assoc-in [trace :disabled] disabled))

           node                         ; Is node already initialized?
           (let [ev (trace i)]
             (recur ss (inc i)
                    (next-sleep pre ev (:sleep node) rels)
                    (wut/subtree (:wut node) ev) args))

           :else
           (let [ev (trace i)
                 node {:wut (if (empty? wut) (wut/singleton ev) wut)
                       :enabled enabled
                       :disabled disabled
                       :sleep sleep}]
             (recur (assoc ss pre node) (inc i)
                    (next-sleep pre ev (:sleep node) rels)
                    (wut/subtree (:wut node) ev) args))))))

(defn mark-as-visited
  "Given a `search-state` and a `trace`, return a `search-state` where all paths
  that are a prefix of `trace` mark it's next event as visited. Marking as
  visited means that the next event is added to the sleep set, and all
  extensions starting with the event is removed."
  [search-state {:keys [trace]}]
  (-> (fn [ss i]
        (let [next-node (ss (subvec trace 0 (inc i)))]
          (if (empty? (:wut next-node))
            (let [pre (subvec trace 0 i)
                  ev (trace i)
                  {:keys [wut]} (ss pre)
                  new-wut (wut/remove-subtree wut ev)]
              (-> (update-in ss [pre :sleep] conj ev)
                  (assoc-in [pre :wut] new-wut)))
            (reduced ss))))
      (reduce search-state (reverse (range (count trace))))))

(defn next-seed [search-state trace]
  (let [{:keys [wut]} (search-state trace)
        branch (wut/min-branch wut)
        seed (into trace branch)]
    (cond (empty? seed) nil
          (search-state seed) (next-seed search-state (pop trace))
          :else seed)))
