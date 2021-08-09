(ns exogenous.dpor.dpor
  (:require [clojure.set :as set]
            [exogenous.dpor.optimal :as optimal]
            [exogenous.dpor.source :as source]
            [exogenous.dpor.wakeup :as wut]))

(defmulti add-trace
  "Given a `search-state` and arguments `args`, containing the `seed-trace`,
  `trace`, `enabled-disabled` and the relations, return a `search-state`
  updated with `trace`. This process consists of initializing the nodes along
  the path of `trace`, adding all extensions that leads to reversing observed
  races and marking the nodes as visited."
  (fn [search-state args] (:backtracking args)))

(defmethod add-trace :optimal
  [search-state args]
  (-> search-state
      (optimal/initialize-new-nodes args)
      (optimal/backtrack-races args)
      (optimal/backtrack-disabled args)
      (optimal/mark-as-visited args)))

(defmethod add-trace :source
  [search-state args]
  (-> search-state
      (source/initialize-new-nodes args)
      (source/backtrack-races args)
      (source/backtrack-disabled args)
      (source/mark-as-visited args)))

(defmethod add-trace :naive
  [search-state {:keys [trace enabled-disabled]}]
  (-> (fn [ss i]
        (let [pre (subvec trace 0 i)
              {:keys [enabled disabled]} (enabled-disabled i)]
          (-> ss
              (assoc-in [pre :enabled] enabled)
              (assoc-in [pre :disabled] disabled))))
      (reduce search-state (range (inc (count trace))))))

(defmulti backtrack
  "Given args containing `search-state`, dispatching on `:backtracking`, return a
  set of seed traces that leads to unexplored executions.

  Given the key `:optimal`, use the extensions to produce seed-traces, leading
  one explored trace per equivalence class.

  Given the key `:naive`, explore all possible executions, based on the enabled
  events in each node."
  :backtracking)

;;; TODO: Improve backtracking

#_(defmethod backtrack :optimal [{:keys [search-state]}]
    (let [candidates (mapcat (fn [[seed-trace {:keys [::optimal/wut]}]]
                               (map (partial into seed-trace) (wut/branches wut))
                               #_(list (into seed-trace (wut/min-branch wut))))
                             search-state)]
      (set (remove search-state candidates))))

(defmethod backtrack :optimal [{:keys [search-state trace]}]
  (if-let [seed (optimal/next-seed search-state trace)]
    #{seed}
    #{}))

#_(defmethod backtrack :source [{:keys [search-state]}]
    (let [candidates (mapcat (fn [[seed-trace {:keys [::source/backset ::source/sleep]}]]
                               (->> (set/difference backset sleep)
                                    (map (partial conj seed-trace))))
                             search-state)]
      (set (remove search-state candidates))))

(defmethod backtrack :source [{:keys [search-state trace]}]
  (if-let [seed (source/next-seed search-state trace)]
    #{seed}
    #{}))

(defmethod backtrack :naive [{:keys [search-state]}]
  (let [candidates (mapcat (fn [[seed-trace {:keys [:enabled]}]]
                             (map (partial conj seed-trace) enabled))
                           search-state)]
    (set (remove search-state candidates))))
