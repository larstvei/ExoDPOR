(ns exogenous.dpor.source
  (:require [clojure.set :as set]
            [exogenous.dpor.common :refer :all]))

(defn update-backtracking
  "Given a `search-state`, a `trace`, with two positions `i` and `j` that
  corresponds to events that are in a reversible race, and relations `rels`,
  return an updated `search-state` that guarantees that a trace in which the
  race is reversed is eventually executed.

  If there is no weak initial for the non-dependent events after `i` in the
  sleep set before `i`, then add an extension that is guaranteed to reverse the
  race."
  [search-state i j {:keys [trace rels]}]
  (let [pre-i (subvec trace 0 i)
        pre-j (subvec trace 0 j)
        ev2 (trace j)
        {:keys [backset sleep]} (search-state pre-i)
        v (conj (not-dep pre-j i rels) ev2)
        initials (initial-set pre-i v rels)]
    (if (empty? (set/intersection (set initials) (set backset)))
      (let [ideal-candidates (set/intersection initials (set sleep))
            ev (or (first ideal-candidates) (first initials))]
        (update-in search-state [pre-i :backset] conj ev))
      search-state)))

(defn backtrack-disabled [search-state {:keys [trace rels] :as args}]
  (-> (fn [ss [i j disabled-event]]
        (let [new-args (assoc-in args [:trace j] disabled-event)]
          (update-backtracking ss i j new-args)))
      (reduce search-state (disabled-races search-state trace rels))))

(defn backtrack-races
  "Given a `search-state`, a `trace`, and relations `rels`, return an updated
  `search-state` where each reversible race will eventually be explored from
  some extension."
  [search-state {:keys [trace rels] :as args}]
  (let [within-bounds? (comp search-state (partial subvec trace 0))
        m (last (take-while within-bounds? (range (count trace))))
        chomped-trace (subvec trace 0 (inc m))]
    (->> (reversible-races search-state chomped-trace rels)
         (reduce (fn [ss [i j]] (update-backtracking ss i j args)) search-state))))

(defn initialize-new-nodes
  "Given a `search-state` and arguments `trace`, `enabled-disabled` and relations
  `rels`, return an updated `search-state` where new nodes in the trace are
  initialized."
  ([search-state args]
   (let [seed (:seed-trace args)
         start (max 0 (dec (count seed)))]
     (initialize-new-nodes search-state start #{} args)))
  ([ss i sleep {:keys [trace enabled-disabled rels] :as args}]
   (let [{:keys [enabled disabled]} (enabled-disabled i)
         pre (subvec trace 0 i)
         node (ss pre)]
     (cond
       ;; We have reached a maximal execution.
       (= i (count trace))
       (-> ss
           (assoc-in [trace :sleep] #{})
           (assoc-in [trace :backset] #{})
           (assoc-in [trace :enabled] enabled)
           (assoc-in [trace :disabled] disabled))

       ;; This case only occurs at the first iteration; that is when ev is the
       ;; initial which `pre` was extended with to make a seed trace. The case
       ;; is necessary for providing the recursive steps with the correct sleep
       ;; set. (Note that we could treat this case in the two-argument function
       ;; body for this function).
       node
       (let [ev (trace i)]
         (recur ss (inc i) (next-sleep pre ev (:sleep node) rels) args))

       ;; This case occurs if the runtime schedules an event that is in the
       ;; sleep set – this can happen in Exo, because the runtime does not keep
       ;; track of a sleep set. Either, there is another event which is not in
       ;; the sleep set which should have been scheduled instead, or it is in a
       ;; sleep blocked state, i.e. that there is no scheduable event that can
       ;; lead to a new non-equivalent run. If there is a another event that is
       ;; not in the sleep set, add this to the backtracking set. Otherwise, we
       ;; stop the recursion, and disregard the rest of the trace.
       (sleep (trace i))
       (if-let [ev (first (set/difference enabled sleep))]
         (let [node {:backset #{ev}
                     :enabled enabled
                     :disabled disabled
                     :sleep sleep}
               next-node-sleep (next-sleep pre ev (:sleep node) rels)]
           (recur (assoc ss pre node) (inc i) next-node-sleep args))
         ss)

       ;; This is the general case in which an event (not in the sleep set) is
       ;; added to the backset, and the sleep set is initialized by the one
       ;; calculated by its parent. We recurse with the initialized node and an
       ;; updated sleep set.
       :else
       (let [ev (trace i)
             node {:backset #{ev}
                   :enabled enabled
                   :disabled disabled
                   :sleep sleep}
             next-node-sleep (next-sleep pre ev (:sleep node) rels)]
         (recur (assoc ss pre node) (inc i) next-node-sleep args))))))

(defn mark-as-visited
  "Given a `search-state` and a `trace`, return a `search-state` where all paths
  that are a prefix of `trace` mark it's next event as visited. Marking as
  visited means that the next event is added to the sleep set, and all
  extensions starting with the event is removed."
  [search-state {:keys [trace]}]
  (-> (fn [ss i]
        (let [next-node (ss (subvec trace 0 (inc i)))]
          (if (empty? (set/difference (set (:backset next-node))
                                      (set (:sleep next-node))))
            (let [pre (subvec trace 0 i)
                  ev (trace i)]
              (update-in ss [pre :sleep] conj ev))
            (reduced ss))))
      (reduce search-state (reverse (range (count trace))))))

(defn next-seed [search-state trace]
  (let [{:keys [backset sleep]} (search-state trace)
        p (first (set/difference backset sleep))
        seed (conj trace p)]
    (cond p seed
          (empty? trace) nil
          :else (next-seed search-state (pop trace)))))
