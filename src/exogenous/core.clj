(ns exogenous.core
  (:require [exogenous.dpor :as dpor]))

(def search-state (atom {}))

(defn submit [trace enabled]
  (swap! search-state dpor/add-trace trace enabled))

(defn explore [sim]
  (apply submit (sim []))
  (loop [backtrack (dpor/backtrack @search-state)]
    (when-not (empty? backtrack)
      (doseq [t backtrack]
        (let [[trace enabled] (sim t)]
          (prn trace)
          (submit trace enabled)))
      (recur (dpor/backtrack @search-state)))))
