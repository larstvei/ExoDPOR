(ns exogenous.core
  (:require [exogenous.dpor :as dpor]
            [exogenous.relations :as rel]))

(def search-state (atom {}))

(defn submit [trace rels]
  (swap! search-state dpor/add-trace trace rels))

(defn explore [sim]
  (reset! search-state {})
  (loop [backtrack [[]]]
    (when-not (empty? backtrack)
      (let [[trace mhb interference] (sim (first backtrack))
            rels (rel/make-rels trace mhb interference)]
        (submit trace rels)
        (recur (dpor/backtrack-depth-first @search-state trace))))))
