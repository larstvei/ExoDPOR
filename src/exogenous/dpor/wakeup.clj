(ns exogenous.dpor.wakeup
  (:require [exogenous.dpor.common :refer :all]
            [exogenous.relations :refer [relates?]]))

(def ordering
  "This procedure associates any value `x` with some arbitrary natural number n,
  such that for all y, if ordering(y) = n then x = y. This is used to provide
  an arbitrary total order on all values, regardless of type. Note that the
  ordering is not stable across runs, as it is dependent on the order in which
  it is first called with some particular value x."
  (let [enumeration (atom 0)]
    (memoize
     (fn [x]
       (swap! enumeration inc)))))

(def empty-wut nil)

(defn singleton [ev]
  {ev empty-wut})

(defn remove-subtree [wut ev]
  (let [res (dissoc wut ev)]
    (when-not (empty? res)
      res)))

(defn subtree [wut ev]
  (let [res (get wut ev)]
    (when-not (empty? res)
      res)))

(defn branches [wut]
  (if (empty? wut)
    '(())
    (mapcat
     (fn [[ev children]]
       (map (partial cons ev) (branches children)))
     wut)))

(defn min-branch [wut]
  (loop [wut wut w []]
    (if (empty? wut)
      w
      (let [e (apply min-key ordering (keys wut))]
        (recur (subtree wut e) (conj w e))))))

(defn weak-initial-seq? [pre w rels [e & v]]
  (or (nil? e)
      (and ((initial-set pre w rels) e)
           (weak-initial-seq? (conj pre e) (remove #{e} w) rels v))
      (and (independent-with? pre e w rels)
           (weak-initial-seq? (conj pre e) w rels v))))

(defn minimal-start [wut pre w {:keys [hb] :as rels}]
  (let [initials (initial-set pre w rels)
        [e & _] (-> (fn [e]
                      (or (initials e)
                          (independent-with? pre e w rels)))
                    (filter (sort-by ordering (keys wut))))]
    (cond (nil? e) ()
          (initials e) (cons e (minimal-start (wut e) (conj pre e) (remove #{e} w) rels))
          :else (cons e (minimal-start (wut e) (conj pre e) w rels)))))

(defn insert [wut pre w rels]
  (let [v (minimal-start wut pre w rels)
        w2 (concat v (remove (set v) w))]
    (assert (weak-initial-seq? pre w rels v))
    (if (empty? (get-in wut v))
      wut
      (assoc-in wut w2 nil))))
