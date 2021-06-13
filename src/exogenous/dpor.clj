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

(defn canonicalize
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

(defn weak-initial-set-1
  "Given a subsequence of a trace `v`, a set of enabled events, and a
  happens-before relation `hb`, return the set of enabled events that has no
  `hb` predecessor in `v`."
  [v enabled {:keys [hb] :as rels}]
  (union (initial-set v rels)
         (set (for [ev enabled
                    :when (empty? (filter #(relates? hb ev %) v))]
                ev))))

(defn weak-initial-set-2
  "Given a subsequence of a trace `v`, a set of enabled events, and a
  happens-before relation `hb`, return the set of enabled events that has no
  `hb` predecessor in `v`."
  [v enabled {:keys [hb interference]}]
  (set (for [ev enabled
             :when (empty? (filter #(relates? interference % ev) v))]
         ev)))

(defn wut-singleton [ev]
  {ev nil})

(defn wut-remove-subtree [wut ev]
  (let [res (dissoc wut ev)]
    (when-not (empty? res)
      res)))

(defn wut-subtree [wut ev]
  (let [res (get wut ev)]
    (when-not (empty? res)
      res)))

(defn wut-branches [wut]
  (if (empty? wut)
    '(())
    (mapcat
     (fn [[ev children]]
       (map (partial cons ev) (wut-branches children)))
     wut)))

(defn wut-min-branch [wut]
  (loop [wut wut w []]
    (if (empty? wut)
      w
      (let [e (apply min-key ordering (keys wut))]
        (recur (wut-subtree wut e) (conj w e))))))

(defn minimal-start [wut w {:keys [hb] :as rels}]
  (let [initials (initial-set w rels)
        [e & _] (-> (fn [e]
                      (or (initials e)
                          (empty? (filter #(relates? hb e %) w))))
                    (filter (sort-by ordering (keys wut))))]
    (cond (nil? e) ()
          (initials e) (cons e (minimal-start (wut e) (remove #{e} w) rels))
          :else (cons e (minimal-start (wut e) w rels)))))

(defn wut-insert [wut w rels]
  (let [canon (canonicalize w rels)
        v (minimal-start wut canon rels)
        w2 (concat v (remove (set v) canon))]
    (if (empty? (get-in wut v))
      (do (println "No insertion," v "is already in wut")
          wut)
      (do
        (println "Inserted:" w2)
        (assoc-in wut w2 nil)))))

(defn update-wakeup-tree
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
        weak-initials (weak-initial-set-1 v enabled rels)
        weak-initials-2 (weak-initial-set-2 v enabled rels)]
    (println "--------------------------------------------------")
    (when (not= weak-initials weak-initials-2)
      (println "Weak initials differ:" weak-initials weak-initials-2))
    (println "Reversible race:" (trace i) (trace j) "\n")
    (println "v:" v)
    (println "sleep:" sleep)
    (println "initials:" (initial-set v rels))
    (println "weak initials:" weak-initials)
    (println "pre: " pre)
    (if (empty? (intersection weak-initials sleep))
      (do
        (println "Attempting to insert")
        (update-in search-state [pre ::wut] wut-insert v rels))
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
  [search-state trace i j {:keys [mhb hb]}]
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
  (for [i (range (count trace))
        j (range (inc i) (count trace))
        :when (reversible-race? search-state trace i j rels)]
    [i j]))

(defn add-to-wakeup-trees
  "Given a `search-state`, a `trace`, and relations `rels`, return an updated
  `search-state` where each reversible race will eventually be explored from
  some extension."
  [search-state {:keys [trace rels]}]
  (->> (reversible-races search-state trace rels)
       (reduce (fn [ss [i j]] (update-wakeup-tree ss trace i j rels)) search-state)))

(defn initialize-node
  "Given an event `ev` a `parent` node, a happens-before relation `hb` and a
  record of `enabled` and `disabled` sets, initialize a new node that can be
  added to the tree.

  The sleep set and the extensions from the node are dependent on the parent
  node, and the event `ev` is the transition between the two nodes.

  The sleep set of the new node is (initially) a subset of the sleep set of the
  parent node, where we keep only the events that are not in happens-after
  relation with `ev`.

  The extensions for the new node are all extensions of the parent node that
  starts with `ev` with `ev` itself removed."
  [prev-ev ev parent {:keys [hb]} {:keys [enabled disabled]}]
  (let [sleep (set (remove #(relates? hb % prev-ev) (::sleep parent)))
        wut (or (wut-subtree (::wut parent) prev-ev)
                (wut-singleton ev))]
    {::wut wut
     ::enabled enabled
     ::disabled disabled
     ::sleep sleep}))

(defn initialize-new-nodes
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
              pre (subvec trace 0 i)
              parent (if (empty? pre) {} (ss (pop pre)))
              prev-ev (last pre)
              node (or (ss pre)
                       (initialize-node
                        prev-ev ev parent rels (enabled-disabled i)))]
          (assoc ss pre node)))
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
  [search-state {:keys [trace]}]
  (-> (fn [ss i]
        (let [next-node (ss (subvec trace 0 (inc i)))]
          (if (empty? (::wut next-node))
            (let [pre (subvec trace 0 i)
                  ev (trace i)
                  {:keys [::wut ::enabled]} (ss pre)
                  new-wut (wut-remove-subtree wut ev)]
              (println "Adding" ev "to sleep at" pre)
              (-> (update-in ss [pre ::sleep] conj ev)
                  (assoc-in [pre ::wut] new-wut)))
            (reduced ss))))
      (reduce search-state (reverse (range (count trace))))))

(defn add-trace
  "Given a `search-state` and arguments `args`, containing the `seed-trace`,
  `trace`, `enabled-disabled` and the relations, return a `search-state`
  updated with `trace`. This process consists of initializing the nodes along
  the path of `trace`, adding all extensions that leads to reversing observed
  races and marking the nodes as visited."
  [search-state args]
  (println "\n##########################################\n")
  (println "adding trace:" (:trace args))
  (println "\nrelations:" (:rels args))
  (println)
  (let [res (-> (initialize-new-nodes search-state args)
                (add-final-node args)
                (add-to-wakeup-trees args)
                (mark-as-visited args))]
    (println "\n")
    (println (sort-by (comp count key) res))
    (println "\n")
    res))

(defmulti backtrack
  "Given args containing `search-state`, dispatching on `:backtracking`, return a
  set of seed traces that leads to unexplored executions.

  Given the key `:optimal`, use the extensions to produce seed-traces, leading
  one explored trace per equivalence class.

  Given the key `:naive`, explore all possible executions, based on the enabled
  events in each node."
  :backtracking)

(defmethod backtrack :optimal [{:keys [search-state]}]
  (let [candidates (mapcat (fn [[seed-trace {:keys [::wut]}]]
                             #_(map (partial into seed-trace) (wut-branches wut))
                             (list (into seed-trace (wut-min-branch wut))))
                           search-state)]
    (set (remove search-state candidates))))

(defmethod backtrack :naive [{:keys [search-state]}]
  (let [candidates (mapcat (fn [[seed-trace {:keys [::enabled]}]]
                             (map (partial conj seed-trace) enabled))
                           search-state)]
    (set (remove search-state candidates))))
