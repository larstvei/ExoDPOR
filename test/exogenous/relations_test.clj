(ns exogenous.relations-test
  (:require [clojure.test :as t]
            [exogenous.relations :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def s #{[:a :b] [:b :c] [:c :d]})
(def r (pairs->rel s))

(t/deftest basic
  (t/is (= {} (pairs->rel #{})))
  (t/is (= {:a #{}, :b #{:a}, :c #{:b}, :d #{:c}} r))
  (t/is (relates? r :a :b))
  (t/is (relates? r :c :d))
  (t/is (not (relates? r :b :a)))
  (t/is (= (dom r) #{:a :b :c :d}))
  (t/is (= s (rel->pairs r)))
  (t/is (= #{[:a :b] [:b :c] [:c :d] [:s :a] [:e :f]}
           (rel->pairs (rel-union r (pairs->rel #{[:s :a] [:e :f]}))))))

(t/deftest transitive
  (t/is (= {:a #{} :b #{:a}, :c #{:a :b} :d #{:a :b :c}}
           (transitive-closure r))))

(t/deftest interference
  (let [s (pairs->rel #{[:a :b] [:b :a] [:c :d] [:d :c]})
        t [:a :b :c :d]]
    (t/is (= {:a #{} :b #{:a} :c #{} :d #{:c}}
             (unsymmetricize-interference t s)))))

(t/deftest relations
  (let [trace [:p1 :q1 :q2 :r1 :r2]
        mhb #{[:q1 :q2] [:r1 :r2]}
        interference #{[:p1 :q2] [:q2 :p1] [:p1 :r2] [:r2 :p1]}]
    (t/is {:mhb {:q2 #{:q1}, :r2 #{:r1}},
           :interference {:q2 #{:p1}, :r2 #{:p1}, :p1 #{}},
           :hb {:q2 #{:q1 :p1}, :r2 #{:p1 :r1}, :p1 #{}}}
          (= (make-rels trace mhb interference)))))

(def gen-elem gen/small-integer)
(def pairs-gen (gen/set (gen/tuple gen-elem gen-elem)))
(def rel-gen (gen/fmap pairs->rel pairs-gen))

(def isomorphic-prop
  (prop/for-all
   [relation pairs-gen]
   (= relation
      (rel->pairs (pairs->rel relation)))))

(def transitive-prop
  (prop/for-all
   [r pairs-gen]
   (let [t (transitive-closure (pairs->rel r))]
     (->> (for [a (dom t) b (dom t) c (dom t)
                :when (and (relates? t a b)
                           (relates? t b c))]
            (relates? t a c))
          (every? identity)))))

(def transitive-prop-2
  (prop/for-all
   [r pairs-gen]
   (let [t (#'exogenous.relations/transitive-closure-floyd-warshall (pairs->rel r))]
     (->> (for [a (dom t) b (dom t) c (dom t)
                :when (and (relates? t a b)
                           (relates? t b c))]
            (relates? t a c))
          (every? identity)))))

(def transitive-equiv-impl-prop
  (prop/for-all
   [r rel-gen]
   (= (transitive-closure r)
      (#'exogenous.relations/transitive-closure-floyd-warshall r))))

(def transitive-relate*
  (prop/for-all
   [pairs pairs-gen]
   (let [r (pairs->rel pairs)
         r* (transitive-closure r)]
     (->> (for [a (dom r) b (dom r)]
            (= (relates? r* a b) (relates*? r a b)))
          (every? identity)))))

(def union-prop
  (prop/for-all
   [r1 pairs-gen
    r2 pairs-gen]
   (= (clojure.set/union r1 r2)
      (rel->pairs
       (rel-union (pairs->rel r1)
                  (pairs->rel r2))))))

(t/deftest generative-tests
  (t/is (= true (:pass? (tc/quick-check 100 isomorphic-prop))))
  (t/is (= true (:pass? (tc/quick-check 100 union-prop))))
  (t/is (= true (:pass? (tc/quick-check 20 transitive-prop))))
  (t/is (= true (:pass? (tc/quick-check 20 transitive-relate*))))
  (t/is (= true (:pass? (tc/quick-check 20 transitive-equiv-impl-prop)))))
