(ns exogenous.relations
  (:require [clojure.set :refer [difference intersection union]]))

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

(defn relates?
  ([r a b] ((or (r b) #{}) a))
  ([t r a b] (and (.contains t b) (relates? r a b))))

(defn dom [r]
  (set (keys r)))

(defn dfs
  ([r s] (dfs (dom r) r s #{}))
  ([t r s] (dfs (set t) r s #{}))
  ([t r s vs] (-> (fn [vs n] (into vs (dfs t r n vs)))
                  (reduce (union (r s) vs)
                          (intersection t (difference (r s) vs))))))

(defn relates*?
  ([r a b]
   (or (relates? r a b)
       (relates? (update r b union (dfs r b)) a b)))
  ([t r a b]
   (or (relates? r a b)
       (relates? (update r b union (dfs (set t) r b)) a b))))

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

(defn hb? [pre {:keys [mhb interference]} ev1 ev2]
  (let [low (.indexOf pre ev1)
        high (.indexOf pre ev2)]
    (when (and (< low high) (not= low -1) (not= high -1))
      (loop [high high seen (union (set (interference ev2)) (set (mhb ev2)))]
        (let [ev (nth pre high)]
          (cond (seen ev1) true
                (= low high) false
                (seen ev) (recur (dec high) (union seen (set (interference ev)) (set (mhb ev))))
                :else (recur (dec high) seen)))))))

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
        hb (->> (make-interference interference trace)
                (rel-union mhb))]
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
