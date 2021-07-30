(ns exogenous.dpor.wakeup
  (:require [exogenous.dpor.common :refer :all]
            [clojure.set :as set]))

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

(defn children [wut]
  (keys wut))

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

(defn initial-seq? [pre [e & v] w rels]
  (or (nil? e)
      (and ((initial-set pre w rels) e)
           (initial-seq? (conj pre e) v (remove #{e} w) rels))))

(defn weak-initial-seq? [pre [e & v] w rels]
  (or (nil? e)
      (and ((initial-set pre w rels) e)
           (weak-initial-seq? (conj pre e) v (remove #{e} w) rels))
      (and (independent-with? pre e w rels)
           (weak-initial-seq? (conj pre e) v w rels))))

(defn minimal-start
  ([wut pre w rels] (minimal-start wut pre [] w rels))
  ([wut pre v w rels]
   (let [events (set (children wut))
         initials (set/intersection events (initial-set pre w rels))
         indep (set (filter #(independent-with? pre % w rels) events))
         ev (first (sort-by ordering (set/union initials indep)))
         pre2 (conj pre ev)
         v2 (conj v ev)]
     (cond (nil? ev) v
           (initials ev) (recur (wut ev) pre2 v2 (remove #{ev} w) rels)
           (indep ev) (recur (wut ev) pre2 v2 w rels)))))

(defn shortest-extension [pre w [e & v] rels]
  (cond (nil? e) w
        (not (.contains w e)) (shortest-extension (conj pre e) w v rels)
        ((initial-set pre w rels) e) (shortest-extension (conj pre e) (remove #{e} w) v rels)
        :else (assert false)))

(defn subsequences-of
  ([w] (subsequences-of w #{[]}))
  ([[e & w] res]
   (if (nil? e)
     '(())
     (let [res (subsequences-of w)]
       (concat res (map (partial cons e) res))))))

(defn shortest-extension-candidates [pre w v rels]
  (->> (subsequences-of w)
       (map (partial concat v))
       (filter #(initial-seq? pre w % rels))))

(defn insert [wut pre w rels enabled]
  (let [v (minimal-start wut pre w rels)
        w2 (concat v (remove (set v) w))]
    (comment
      (assert (weak-initial-seq? pre v w rels))
      (assert (weak-initial-seq? pre w v rels))
      (assert (initial-seq? pre w w2 rels))
      (let [wi1 (weak-initial-set pre w enabled rels)
            wi2 (weak-initial-set pre w2 enabled rels)]
        (assert (clojure.set/subset? wi2 wi1)))
      (let [w2-alternative (concat v (shortest-extension pre w v rels))
            candidates (shortest-extension-candidates pre w v rels)
            w2-brute (apply min-key count candidates)]
        (assert (= w2 w2-alternative w2-brute))))
    (if (empty? (get-in wut v))
      wut
      (assoc-in wut w2 nil))))
