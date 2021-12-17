(ns exogenous.dpor.common
  (:require [clojure.set :as set]
            [exogenous.relations :refer [hb? relates?]]))

(defn not-dep
  "Given a `trace`, an index `i` where event `ev` occurs, and a happens-before
  relation `hb`, return a vector consisting of elements that occur after `ev`
  but does not happen-after `ev`."
  [trace i rels]
  (let [[ev & t] (subvec trace i)]
    (filterv #(not (hb? trace rels ev %)) t)))

(defn disables?
  "Given a `search-state`, a trace `pre` and two events `ev1` and `ev2`, return
  true if `ev1` disables `ev2` after path. Assume `ev1` occurs directly after
  `pre`. Then `ev1` disables `ev2` if `ev2` was enabled in the state before
  `ev1` was executed, and disabled after."
  [search-state pre ev1 ev2]
  (let [{enabled-before :enabled} (search-state pre)
        {disabled-after :disabled} (search-state (conj pre ev1))]
    (and (enabled-before ev2) (disabled-after ev2))))

(defn independent-with? [pre ev w {:keys [hb mhb interference] :as rels}]
  (let [dom (vec (concat pre [ev] w))]
    (every? #(not (hb? dom rels ev %)) w)))

(defn initial-set
  "Given a subsequence of a trace `v` and a happens-before relation `hb`, return
  the set of events that has no `hb` predecessor in `v`."
  [pre v rels]
  (let [w (into pre v)]
    (set (for [ev v
               :when (every? #(not (hb? w rels % ev)) v)]
           ev))))

(defn weak-initial-set
  "Given a subsequence of a trace `v`, a set of enabled events, and a
  happens-before relation `hb`, return the set of enabled events that has no
  `hb` predecessor in `v`."
  [pre v enabled {:keys [hb] :as rels}]
  (set/union (initial-set pre v rels)
             (set (for [ev enabled
                        :when (independent-with? pre ev v rels)]
                    ev))))

(defn reversible-race?
  "Given a `search-state`, a `trace`, two positions `i` < `j` and relations `mhb`
  and `hb`, return true if the event at `i` is in a reversible race with the
  event at `j`.

  Two events are in a race if they are in `hb` and that they may occur
  simultaneously in an otherwise equivalent execution (i.e. there is no event
  between `i` and `j` that are in `hb` with both).

  The race is reversible if the events are not in `mhb` and that the event at
  `j` is not blocked by the event at `i`."
  [search-state trace i j {:keys [mhb hb] :as rels}]
  (let [pre (subvec trace 0 i)
        {:keys [disabled]} (search-state pre)
        ev1 (trace i)
        ev2 (trace j)]
    (and (not (relates? mhb ev1 ev2))
         (not (disabled ev2))
         (hb? trace rels ev1 ev2)
         (empty? (for [k (range (inc i) j)
                       :let [ev-mid (trace k)]
                       :when (and (hb? trace rels ev1 ev-mid)
                                  (hb? trace rels ev-mid ev2))]
                   k)))))

(defn disabled-races [search-state trace rels]
  (for [i (range (count trace))
        j (range (inc i) (inc (count trace)))
        :let [pre1 (subvec trace 0 i)
              pre2 (subvec trace 0 j)
              ev1 (trace i)
              {:keys [:disabled]} (search-state pre2)]
        disabled-event disabled
        :when (disables? search-state pre1 ev1 disabled-event)]
    [i j disabled-event]))

(defn reversible-races
  "Given a `search-state`, a `trace`, and relations `rels`, return the all pairs
  of indices that are in a reversible race."
  [search-state trace rels]
  (for [i (range (count trace))
        j (range (inc i) (count trace))
        :when (reversible-race? search-state trace i j rels)]
    [i j]))

(defn next-sleep [pre ev sleep {:keys [hb] :as rels}]
  (set (filter #(independent-with? pre ev [%] rels) sleep)))
