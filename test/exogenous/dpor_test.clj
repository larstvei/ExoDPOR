(ns exogenous.dpor-test
  (:require [exogenous.dpor :refer :all]
            [exogenous.relations :as rel]
            [clojure.test :as t]))

(def trace [:p1 :q1 :q2 :r1 :r2])
(def pre [:p1])
(def w [:q1 :q2])
(let [mhb-raw #{[:q1 :q2] [:r1 :r2]}
      interference-raw #{[:p1 :q2] [:p1 :r2]}]
  (def rels (rel/make-rels trace mhb-raw interference-raw))
  (def mhb (:mhb rels))
  (def interference (:interference interference)))
(def enabled-sets [#{:q1 :p1 :r1} #{:q1 :r1} #{:q2 :r1} #{:r1} #{:r2} #{}])

(t/deftest initials
  (t/is (= #{:q1} (initial-set pre w rels))))

(t/deftest enabled-test
  (t/is
   (= enabled-sets
      (mapv
       (fn [i] (enabled-after trace i rels))
       (range (count enabled-sets))))))

(t/deftest reversible-race
  (t/is (= (->> (for [n (range (count trace))
                      j (range (inc n))
                      :when (reversible-race? trace n j rels)]
                  [(trace j) (trace n)])
                (into #{}))
           #{[:p1 :q2] [:p1 :r2]})))

(t/deftest add-a-trace
  (let [result (add-trace {} trace rels)]
    (t/is (= {[]
              {:backset #{:q1 :p1}, :enabled #{:q1 :p1 :r1}, :blocked #{}, :sleep #{:p1}},

              [:p1]
              {:sleep #{:q1}, :backset #{:q1}, :enabled #{:q1 :r1}, :blocked #{}},

              [:p1 :q1]
              {:sleep #{:q2}, :backset #{:q2}, :enabled #{:q2 :r1}, :blocked #{}},

              [:p1 :q1 :q2]
              {:sleep #{:r1}, :backset #{:r1}, :enabled #{:r1}, :blocked #{}},

              [:p1 :q1 :q2 :r1]
              {:sleep #{:r2}, :backset #{:r2}, :enabled #{:r2}, :blocked #{}},

              [:p1 :q1 :q2 :r1 :r2]
              {:sleep #{}, :enabled #{}}}
             result))
    (t/is (= '([:q1])
             (backtrack result)))))
