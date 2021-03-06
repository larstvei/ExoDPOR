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

(defn order-preserving-transitive-closure [r sequence]
  (-> (fn [r n]
        (let [i (.indexOf sequence n)
              elems (filter #(< i (.indexOf % n)) (dfs r n))]
          (update r n union)))
      (reduce r (dom r))))

(defn rel-union [r1 r2]
  (-> (fn [r k]
        (reduce #(relate %1 %2 k) r (r2 k)))
      (reduce r1 (keys r2))))

(defn unsymmetricize-interference [trace r]
  (-> (fn [r i]
        (let [e (trace i)
              occur-after (set (subvec trace i))]
          (update r e (fnil difference #{}) occur-after)))
      (reduce r (range (count trace)))))

(defn enabled-candidates [domain visited r]
  (into #{} (filter #(empty? (difference (r %) visited)) domain)))

(defn linerize
  ([domain hb] (linerize domain hb []))
  ([domain hb trace]
   (if-let [e (first (enabled-candidates domain (set trace) hb))]
     (recur (disj domain e) hb (conj trace e))
     trace)))

(defn make-mhb [pairs trace]
  (order-preserving-transitive-closure (pairs->rel pairs) trace))

(defn make-interference [pairs trace]
  (->> (pairs->rel pairs)
       (unsymmetricize-interference trace)
       (transitive-closure)))

(defn make-rels [trace mhb interference]
  (let [mhb (make-mhb mhb trace)
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
