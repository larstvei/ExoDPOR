(ns exogenous.core
  (:require [exogenous.dpor :as dpor]))

(def search-state (atom {}))

(defn submit [trace enabled mhb interference]
  (swap! search-state dpor/add-trace trace enabled mhb interference))

(defn explore [sim]
  (reset! search-state {})
  (loop [backtrack [[]]]
    (when-not (empty? backtrack)
      (let [[trace enabled mhb interference] (sim (first backtrack))]
        (submit trace enabled mhb interference)
        (recur (dpor/backtrack-depth-first @search-state trace))))))
