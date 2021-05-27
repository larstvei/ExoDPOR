(ns exogenous.dpor
  (:require [clojure.set :refer [union difference intersection]]
            [exogenous.relations :refer [relates?]]))

(def ordering
  "This procedure associates any value `x` with some arbitrary natural number n,
  such that for all y, if ordering(y) = n then x = y. This is used to provide
  an arbitrary total order on all values, regardless of type. Note that the
  ordering is not stable across runs, as it is dependent on the order in which
  it is first called with some particular value x."
  (let [enumeration (atom 0)]
    (memoize
     (fn [x]
       (swap! enumeration inc)))))

(defn canon
  "Given a subsequence of a trace `v`, and a happens-before relation `hb` for
  that trace, return a canonical permutation of `v` that respects the `hb`. We
  obtain such a canonical permutation by using the (arbitrary) total ordering
  provided by `ordering`, iteratively selecting the minimal element that
  respects `hb`."
  [v {:keys [hb]}]
  (loop [w [] remaining (set v)]
    (if (empty? remaining)
      w
      (let [e (->> (for [e remaining
                         :when (empty? (filter #(relates? hb % e) remaining))]
                     e)
                   (apply min-key ordering))]
        (recur (conj w e) (disj remaining e))))))

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
  [v enabled {:keys [hb]}]
  (set (for [ev enabled
             :when (empty? (filter #(relates? hb % ev) v))]
         ev)))

(defn update-extensions
  "Given a `search-state`, a `trace`, with two positions `i` and `j` that
  corresponds to events that are in a reversible race, and relations `rels`,
  return an updated `search-state` that guarantees that a trace in which the
  race is reversed is eventually executed.

  If there is no weak initial for the non-dependent events after `i` in the
  sleep set before `i`, then add an extension that is guaranteed to reverse the
  race."
  [search-state trace i j rels]
  (let [pre (subvec trace 0 i)
        ev2 (trace j)
        {:keys [::enabled ::sleep]} (search-state pre)
        v (conj (not-dep trace i rels) ev2)
        weak-initials (weak-initial-set v enabled rels)]
    (if (empty? (intersection weak-initials sleep))
      (update-in search-state [pre ::extensions] conj (canon v rels))
      search-state)))

(defn reversible-race?
  "Given a `search-state`, a `trace`, two positions `i` < `j` and relations `mhb`
  and `hb`, return true if the event at `i` is in a reversible race with the
  event at `j`.

  Two events are in a race if they are in `hb` and that they may occur
  simultaneously in an otherwise equivalent execution (i.e. there is no event
  between `i` and `j` that are in `hb` with both).

  The race is reversible if the events are not in `mhb` and that the event at
  `j` is not blocked by the event at `i`."
  [search-state trace i j {:keys [mhb interference hb]}]
  (let [pre (subvec trace 0 i)
        ev1 (trace i)
        ev2 (trace j)
        {:keys [::disabled]} (search-state pre)]
    (and (not (disabled ev2))            ; <- unsure about this
         (not (relates? mhb ev1 ev2))
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
  (for [j (range (count trace))
        i (range j)
        :when (reversible-race? search-state trace i j rels)]
    [i j]))

(defn add-extensions
  "Given a `search-state`, a `trace`, and relations `rels`, return an updated
  `search-state` where each reversible race will eventually be explored from
  some extension."
  [search-state {:keys [trace rels]}]
  (->> (reversible-races search-state trace rels)
       (reduce (fn [s [i j]] (update-extensions s trace i j rels)) search-state)))

(defn initialize-node
  "Given an event `ev` a `parent` node, a happens-before relation `hb` and a
  record of `enabled` and `disabled` sets, initialize a new node that can be
  added to the tree.

  The sleep set and the extensions from the node are dependent on the parent
  node, and the event `ev` is the transition between the two nodes.

  The sleep set of the new node is (initially) a subset of the sleep set of the
  parent node, where we only keep the entries that are not in happens-after
  relation with `ev`.

  The extensions for the new node are all extensions of the parent node that
  starts with `ev` with `ev` itself removed."
  [ev parent {:keys [hb]} {:keys [enabled disabled]}]
  (let [sleep (set (remove #(relates? hb ev %) (::sleep parent)))
        extensions (->> (::extensions parent)
                        (filter #(= ev (first %)))
                        (map (comp vec rest))
                        (remove empty?)
                        (into #{}))]
    {::extensions extensions
     ::enabled enabled
     ::disabled disabled
     ::sleep sleep}))

(defn initialize-new-nodesn
  "Given a `search-state` and arguments `trace`, `enabled-disabled` and relations
  `rels`, return an updated `search-state` where new nodes in the trace are
  initialized.

  Note that we walk the entire trace, even though it would be sufficient to
  start at the longest prefix of `trace` that already is associated with a node
  (i.e. the point from which we extended the trace). Any node already in the
  tree is left unmodified."
  [search-state {:keys [trace enabled-disabled rels]}]
  (-> (fn [ss i]
        (let [ev (trace i)
              path (subvec trace 0 i)
              parent (if (empty? path) {} (ss (pop path)))
              node (or (ss path)
                       (initialize-node
                        ev parent rels (enabled-disabled i)))]
          (assoc ss path node)))
      (reduce search-state (range (count trace)))))

(defn add-final-node
  "Given a `search-state`, a `trace` and `enabled-disabled` entries, return an
  updated `search-state` where a final node with enabled- and disabled sets are
  added.

  This is treated as a special case because there is one more state than there
  are transitions (or events).

  At this point, the enabled set is guaranteed to be empty (given that the
  execution is maximal), and a non-empty disabled set indicates a deadlock."
  [search-state {:keys [trace enabled-disabled]}]
  (let [{:keys [enabled disabled]} (enabled-disabled (count trace))]
    (-> search-state
        (assoc-in [trace ::enabled] enabled)
        (assoc-in [trace ::disabled] disabled))))

(defn mark-as-visited
  "Given a `search-state` and a `trace`, return a `search-state` where all paths
  that are a prefix of `trace` mark it's next event as visited. Marking as
  visited means that the next event is added to the sleep set, and all
  extensions starting with the event is removed."
  [search-state trace]
  (-> (fn [ss i]
        (let [next-node (ss (subvec trace 0 (inc i)))]
          (if (empty? (::extensions next-node))
            (let [pre (subvec trace 0 i)
                  ev (trace i)
                  {:keys [::extensions]} (search-state pre)
                  new-extensions (set (remove #(= ev (first %)) extensions))]
              (-> (update-in ss [pre ::sleep] conj ev)
                  (assoc-in [pre ::extensions] new-extensions)))
            ss)))
      (reduce search-state (range (count trace)))))

(defn add-trace
  "Given a `search-state` and arguments `args`, containing the `seed-trace`,
  `trace`, `enabled-disabled` and the relations, return a `search-state`
  updated with `trace`. This process consists of initializing the nodes along
  the path of `trace`, adding all extensions that leads to reversing observed
  races and marking the nodes as visited."
  [search-state {:keys [trace] :as args}]
  (-> (initialize-new-nodes search-state args)
      (add-final-node args)
      (add-extensions args)
      (mark-as-visited trace)))

(defmulti backtrack
  "Given args containing `search-state`, dispatching on `:backtracking`, return a
  set of seed traces that leads to unexplored executions.

  Given the key `:optimal`, use the extensions to produce seed-traces, leading
  one explored trace per equivalence class.

  Given the key `:naive`, explore all possible executions, based on the enabled
  events in each node."
  :backtracking)

(defmethod backtrack :optimal [{:keys [search-state]}]
  (let [candidates (mapcat (fn [[seed-trace {:keys [::extensions]}]]
                             (map (partial into seed-trace) extensions))
                           search-state)]
    (set (remove search-state candidates))))

(defmethod backtrack :naive [{:keys [search-state]}]
  (let [candidates (mapcat (fn [[seed-trace {:keys [::enabled]}]]
                             (map (partial conj seed-trace) enabled))
                           search-state)]
    (set (remove search-state candidates))))
