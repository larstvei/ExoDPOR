(ns exogenous.relations
  (:require [clojure.set :refer [difference union]]))

(defn relate [r a b]
  (-> (update r b (fnil conj #{}) a)
      (update a (fnil union #{}) #{})))

(defn pairs->rel
  ([pairs] (pairs->rel {} pairs))
  ([r pairs] (reduce (partial apply relate) r pairs)))

(defn rel->pairs [r]
  (-> (fn [s k]
        (into s (for [v (r k)] [v k])))
      (reduce #{} (keys r))))

(defn relates? [r a b]
  ((or (r b) #{}) a))

(defn dom [r]
  (set (keys r)))

(defn dfs
  ([r s] (dfs r s #{}))
  ([r s vs] (-> (fn [vs n] (into vs (dfs r n vs)))
                (reduce (union (r s) vs) (difference (r s) vs)))))

(defn relates*? [r a b]
  ;; TODO: Make order-preserving version for ABS
  (or (relates? r a b)
      (relates? (update r b union (dfs r b)) a b)))

(defn transitive-closure [r]
  (-> (fn [r n]
        (update r n union (dfs r n)))
      (reduce r (dom r))))

(defn rel-union [r1 r2]
  (-> (fn [r k]
        (reduce #(relate %1 %2 k) r (r2 k)))
      (reduce r1 (keys r2))))

(defn unsymmetricize-interference [trace r]
  ;; What do we want to do with the events that don't appear in the trace? It
  ;; seems reasonable to consider the events that aren't scheduled at all as
  ;; happening after all events that reside in the trace. The choice may be
  ;; arbitrary.
  (let [blocked (difference (dom r) (set trace))]
    (-> (fn [r i]
          (let [e (trace i)
                occur-after (into blocked (subvec trace i))]
            (update r e (fnil difference #{}) occur-after)))
        (reduce r (range (count trace))))))

(defn enabled-candidates [domain visited r]
  (into #{} (filter #(empty? (difference (r %) visited)) domain)))

(defn linerize
  ([domain hb] (linerize domain hb []))
  ([domain hb trace]
   (if-let [e (first (enabled-candidates domain (set trace) hb))]
     (recur (disj domain e) hb (conj trace e))
     trace)))

(defn make-interference [interference trace]
  (unsymmetricize-interference trace interference))

(defn make-rels [trace mhb interference]
  (let [mhb (transitive-closure (if (map? mhb) mhb (pairs->rel mhb)))
        interference (if (map? interference) interference (pairs->rel interference))
        interference (make-interference interference trace)
        hb (transitive-closure (rel-union mhb interference))]
    {:mhb mhb :interference interference :hb hb}))

;;; Surprisingly slow, keep for reference.
(defn- transitive-closure-floyd-warshall [r]
  (let [d (dom r)]
    (-> (fn [r k]
          (->> (for [i d
                     j d
                     :when (and (relates? r i k)
                                (relates? r k j))]
                 [i j])
               (pairs->rel r)))
        (reduce r d))))
