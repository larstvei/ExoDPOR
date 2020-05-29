(ns exogenous.core
  (:require [exogenous.dpor :as dpor]
            [exogenous.relations :as rel]
            [clojure.core.async :as async]))

(def default-options {:strategy :random
                      :backtracking :backsets
                      :workers (* 2 (.availableProcessors
                                     (Runtime/getRuntime)))})

(def search-state (atom {}))

(defn submit! [seed-trace trace rels]
  (swap! search-state dpor/add-trace seed-trace trace rels))

(defn execute [sim seed-trace]
  (let [{:keys [trace mhb interference]} (sim seed-trace)
        rels (rel/make-rels trace mhb interference)]
    (assoc {:seed-trace seed-trace :trace trace} :rels rels)))

(defn- longest-common-prefix [[x & xs] [y & ys]]
  (if (and x y (= x y))
    (inc (longest-common-prefix xs ys))
    0))

(defmulti select :strategy)

(defmethod select :depth-first [{:keys [trace candidates n]}]
  (set (take n (sort-by (comp - (partial longest-common-prefix trace)) candidates))))

(defmethod select :breadth-first [{:keys [candidates n]}]
  (set (take n (sort-by count candidates))))

(defmethod select :random [{:keys [candidates n]}]
  (set (take n (shuffle candidates))))

(defmethod select :default [options]
  ((:strategy options) options))

;; Synchronous variant
(defn explore-synchronous
  ([sim] (explore-synchronous sim {}))
  ([sim options]
   (reset! search-state {})
   (let [options (merge default-options options)]
     (loop [backtrack [[]] stats {}]
       (if (empty? backtrack)
         stats
         (let [seed-trace (first backtrack)
               {:keys [trace mhb interference]} (sim seed-trace)
               rels (rel/make-rels trace mhb interference)]
           (submit! seed-trace trace rels)
           (let [candidates (dpor/backtrack (assoc options :search-state @search-state))]
             (recur (select (merge options {:n 1 :candidates candidates}))
                    (update stats (:hb rels) conj trace)))))))))

;; Parallelized variant
(defn explore
  ([sim] (explore sim {}))
  ([sim options]
   (reset! search-state {})
   (let [options (merge default-options options)
         c (async/chan)]
     (loop [seeds #{[]}
            active-jobs #{}
            ;; TODO: record number of candidate seed traces during search
            stats {}]
       (cond
         ;; If we are saturated, block and continue when a job is completed
         (and (not (empty? active-jobs))
              (or (= (count active-jobs) (:workers options))
                  (empty? seeds)))
         (let [{:keys [:seed-trace :trace :rels]} (async/<!! c)
               _ (submit! seed-trace trace rels)
               candidates (dpor/backtrack
                           (assoc options :search-state @search-state))]
           (recur (set (remove active-jobs candidates))
                  (disj active-jobs seed-trace)
                  (update stats (:hb rels) conj trace)))

         ;; We are not saturated, and there is more work to do
         (not (empty? seeds))
         (let [n (- (:workers options) (count active-jobs))
               candidates (select (merge options {:candidates seeds :n n}))]
           (doseq [seed candidates]
             (.start
              (Thread.
               (fn [] (let [res (execute sim seed)]
                        (async/>!! c res))))))
           (recur (set (remove candidates seeds))
                  (into active-jobs candidates)
                  stats))

         (and (empty? seeds) (empty? active-jobs))
         stats)))))
