(ns exogenous.dpor.common
  (:require [clojure.set :as set]
            [exogenous.relations :refer [relates?]]))

(defn not-dep
  "Given a `trace`, an index `i` where event `ev` occurs, and a happens-before
  relation `hb`, return a vector consisting of elements that occur after `ev`
  but does not happen-after `ev`."
  [trace i {hb :hb}]
  (let [ev (trace i)
        t (subvec trace (inc i))]
    (filterv #(not (relates? hb ev %)) t)))

(defn disables?
  "Given a `search-state`, a trace `pre` and two events `ev1` and `ev2`, return
  true if `ev1` disables `ev2` after path. Assume `ev1` occurs directly after
  `pre`. Then `ev1` disables `ev2` if `ev2` was enabled in the state before
  `ev1` was executed, and disabled after."
  [search-state pre ev1 ev2]
  (let [{enabled-before ::enabled} (search-state pre)
        {disabled-after ::disabled} (search-state (conj pre ev1))]
    (and (enabled-before ev2) (disabled-after ev2))))

(defn initial-set
  "Given a subsequence of a trace `v` and a happens-before relation `hb`, return
  the set of events that has no `hb` predecessor in `v`."
  [v {hb :hb}]
  (set (for [ev v
             :when (empty? (filter #(relates? hb % ev) v))]
         ev)))

(defn weak-initial-set
  "Given a subsequence of a trace `v`, a set of enabled events, and a
  happens-before relation `hb`, return the set of enabled events that has no
  `hb` predecessor in `v`."
  [v enabled {:keys [hb] :as rels}]
  (set/union (initial-set v rels)
             (set (for [ev enabled
                        :when (empty? (filter #(relates? hb ev %) v))]
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
  [search-state trace i j {:keys [mhb hb]}]
  (let [pre (subvec trace 0 i)
        ev1 (trace i)
        ev2 (trace j)]
    (and (not (relates? mhb ev1 ev2))
         (relates? hb ev1 ev2)
         (empty? (for [k (range (inc i) j)
                       :let [ev-mid (trace k)]
                       :when (and (relates? hb ev1 ev-mid)
                                  (relates? hb ev-mid ev2))]
                   k)))))

(defn reversible-races
  "Given a `search-state`, a `trace`, and relations `rels`, return the all pairs
  of indices that are in a reversible race."
  [search-state trace rels]
  (for [i (range (count trace))
        j (range (inc i) (count trace))
        :when (reversible-race? search-state trace i j rels)]
    [i j]))

(defn next-sleep [ev sleep {:keys [hb]}]
  (set (remove #(relates? hb ev %) sleep)))