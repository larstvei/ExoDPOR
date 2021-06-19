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

(defn canonicalize
  "Given a subsequence of a trace `v`, and a happens-before relation `hb` for
  that trace, return a canonical permutation of `v` that respects the `hb`. We
  obtain such a canonical permutation by using the (arbitrary) total ordering
  provided by `ordering`, iteratively selecting the minimal element that
  respects `hb`."
  [v {:keys [hb]}]
  (loop [w [] remaining (set v)]
    (if (empty? remaining)
      w
      (let [e (->> (for [e remaining
                         :when (empty? (filter #(relates? hb % e) remaining))]
                     e)
                   (apply min-key ordering))]
        (recur (conj w e) (disj remaining e))))))

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

(defn minimal-start [wut w {:keys [hb] :as rels}]
  (let [initials (initial-set w rels)
        [e & _] (-> (fn [e]
                      (or (initials e)
                          (empty? (filter #(relates? hb e %) w))))
                    (filter (sort-by ordering (keys wut))))]
    (cond (nil? e) ()
          (initials e) (cons e (minimal-start (wut e) (remove #{e} w) rels))
          :else (cons e (minimal-start (wut e) w rels)))))

(defn insert [wut w rels]
  (let [canon (canonicalize w rels)
        v (minimal-start wut canon rels)
        w2 (concat v (remove (set v) canon))]
    (if (empty? (get-in wut v))
      wut
      (assoc-in wut w2 nil))))
