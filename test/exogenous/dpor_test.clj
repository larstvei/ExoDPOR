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
(def enabled-disabled
  [{:enabled #{:q1 :p1 :r1} :disabled #{}}
   {:enabled #{:q1 :r1} :disabled #{}}
   {:enabled #{:q2 :r1} :disabled #{}}
   {:enabled #{:r1} :disabled #{}}
   {:enabled #{:r2} :disabled #{}}
   {:enabled #{} :disabled #{}}])

(t/deftest initials
  (t/is (= #{:q1} (initial-set w rels))))

(t/deftest add-a-trace
  (let [result (add-trace {} [] trace enabled-disabled rels)]
    (t/is (= {[]
              {:backset #{:q1 :p1}, :enabled #{:q1 :p1 :r1}, :disabled #{}, :sleep #{:p1}},

              [:p1]
              {:sleep #{:q1}, :backset #{:q1}, :enabled #{:q1 :r1}, :disabled #{}},

              [:p1 :q1]
              {:sleep #{:q2}, :backset #{:q2}, :enabled #{:q2 :r1}, :disabled #{}},

              [:p1 :q1 :q2]
              {:sleep #{:r1}, :backset #{:r1}, :enabled #{:r1}, :disabled #{}},

              [:p1 :q1 :q2 :r1]
              {:sleep #{:r2}, :backset #{:r2}, :enabled #{:r2}, :disabled #{}},

              [:p1 :q1 :q2 :r1 :r2]
              {:sleep #{}, :enabled #{}, :disabled #{}}}
             result))
    (t/is (= #{[:q1]}
             (backtrack {:backtracking :backsets
                         :trace trace
                         :search-state result})))
    (t/is (= (->> (for [n (range (count trace))
                        j (range (inc n))
                        :when (reversible-race? result trace n j rels)]
                    [(trace j) (trace n)])
                  (into #{}))
             #{[:p1 :q2] [:p1 :r2]}))))
