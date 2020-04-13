(ns exogenous.core
  (:require [exogenous.dpor :as dpor]
            [exogenous.relations :as rel]))

(def search-state (atom {}))

(defn submit [prefix trace rels]
  (swap! search-state dpor/add-trace prefix trace rels))

(defn explore
  ([sim] (explore sim {}))
  ([sim options]
   (reset! search-state {})
   (loop [backtrack [[]] executed {}]
     (if (empty? backtrack)
       [(count executed) (reduce + (map count (vals executed)))]
       (let [prefix (first backtrack)
             {:keys [trace mhb interference]} (sim prefix)
             rels (rel/make-rels trace mhb interference)]
         (submit prefix trace rels)
         (recur (dpor/backtrack (merge {:strategy :depth-first
                                        :search-state @search-state
                                        :trace trace}
                                       options))
                (update executed (:hb rels) conj trace)))))))
