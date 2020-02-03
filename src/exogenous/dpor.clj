(ns exogenous.dpor
  (:require [clojure.set :refer [union difference]]))

(defn add-trace [search-state trace enabled]
  (-> (fn [s i]
        (let [state {:backset #{} :enabled (enabled i) :blocked #{} :sleep #{(trace i)}}]
          (update s (subvec trace 0 i) (partial merge-with union) state)))
      (reduce search-state (range (count trace)))
      (update trace (partial merge-with union) {:enabled (last enabled)})))

(defn backtrack [search-state]
  (-> (fn [[trace {:keys [:enabled :sleep]}]]
        (map (partial conj trace) (difference enabled sleep)))
      (mapcat search-state)))
