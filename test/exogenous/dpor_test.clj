(ns exogenous.dpor-test
  (:require [exogenous.dpor :refer :all]
            [clojure.test :as t]))

(def trace [:p1 :q1 :q2 :r1 :r2])
(def pre [:p1])
(def w [:q1 :q2])
(def mhb #{[:q1 :q2] [:r1 :r2]})
(def interference #{[:p1 :q2] [:p1 :r2]})
(def enabled-sets [#{:q1 :p1 :r1} #{:q1 :r1} #{:q2 :r1} #{:r1} #{:r2} #{}])

(t/deftest initials
  (t/testing "Initials"
    (t/is (= #{:q1} (initial-set pre w mhb)))))

(t/deftest reversible-race
  (t/testing "reversible races"
    (t/is (= (->> (for [n (range (count trace))
                        j (range (inc n))
                        :when (reversible-race? trace n j mhb interference)]
                    [(trace j) (trace n)])
                  (into #{}))
             #{[:p1 :q2] [:p1 :r2]}))))

(t/deftest add-a-trace
  (t/testing "adding a trace"
    (t/is (= (add-trace {} trace enabled-sets mhb interference)
             {[]
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
              {:sleep #{}, :enabled #{}}}))))
