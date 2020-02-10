(ns exogenous.core
  (:require [exogenous.dpor :as dpor]))

(def search-state (atom {}))

(defn submit [trace enabled mhb interference]
  (swap! search-state dpor/add-trace trace enabled mhb interference))

(defn explore [sim]
  (apply submit (sim []))
  (loop [backtrack (dpor/backtrack @search-state)]
    (when-not (empty? backtrack)
      (doseq [t backtrack]
        (apply submit (sim t)))
      (recur (dpor/backtrack @search-state)))))
