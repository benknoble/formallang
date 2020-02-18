(ns formallang.dfa-test
  (:require
   [clojure.test :as t]
   [formallang.dfa :as dfa]))

(def even-bs
  (dfa/map->dfa
   {:K #{0 1}
    :Sigma #{\a \b}
    :delta {0 {\a 0 \b 1}
            1 {\a 1 \b 0}}
    :s 0
    :F #{0}}))

(def simplest
  {:K #{0}
   :Sigma #{\a}
   :delta {0 {\a 0}}
   :s 0
   :F #{0}})

(t/deftest test-dfa
  (t/are [d] (thrown? AssertionError (dfa/map->dfa d))
    (assoc simplest :K #{1})
    (assoc simplest :Sigma #{\b})
    (assoc-in simplest [:delta 0] {\b 0})
    (assoc simplest :s 1)
    (assoc simplest :F #{1})
    (assoc even-bs :delta {0 {\a 0} 1 {\b 0}})))

(t/deftest test-step
  (t/are [s1 d s2] (= s1 (dfa/step1 d s2))
    {:state 0 :word (seq "b")} even-bs {:state 0 :word "ab"}
    {:state 0 :word dfa/e} even-bs {:state 1 :word "b"}
    {:state nil :word nil} even-bs {:state 0 :word ""}
    {:state nil :word nil} even-bs {:state 0 :word "c"}
    {:state nil :word nil} even-bs {:state 2 :word "a"}))

(t/deftest test-step-until
  (t/is (= {:state 1 :word (seq "b")}
           (dfa/step-until #(= 1 (:state %))
                           even-bs
                           {:state (:s even-bs) :word "bb"}))))

(t/deftest test-step*
  (t/are [s1 d s2] (= s1 (dfa/step* d s2))
    {:state 0 :word dfa/e} even-bs {:state (:s even-bs) :word "bb"}
    {:state 1 :word dfa/e} even-bs {:state (:s even-bs) :word "ab"}))

(t/deftest test-accepts-state?
  (t/are [d s] (dfa/accepts-state? d s)
    even-bs 0)
  (t/are [d s] (not (dfa/accepts-state? d s))
    even-bs 1
    even-bs 2
    even-bs nil))

(t/deftest test-accepts-conf?
  (t/are [d s] (dfa/accepts-conf? d s)
    even-bs {:state 0 :word dfa/e})
  (t/are [d s] (not (dfa/accepts-conf? d s))
    even-bs {:state 0 :word ""}
    even-bs {:state 0 :word "a"}
    even-bs {:state 1 :word dfa/e}
    even-bs {:state 2 :word dfa/e}
    even-bs {:state nil :word dfa/e}))

(t/deftest test-yields1?
  (t/are [d s1 s2] (dfa/yields1? d s1 s2)
    even-bs {:state 0 :word "a"} {:state 0 :word dfa/e}
    even-bs {:state 0 :word "b"} {:state 1 :word dfa/e}
    even-bs {:state 0 :word "aba"} {:state 0 :word (seq "ba")})
  (t/are [d s1 s2] (not (dfa/yields1? d s1 s2))
    even-bs {:state 0 :word "a"} {:state 1 :word dfa/e}))

(t/deftest test-yields*?
  (t/are [d s1 s2] (dfa/yields*? d s1 s2)
    even-bs {:state 0 :word "aba"} {:state 0 :word (seq "ba")}
    even-bs {:state 0 :word "aba"} {:state 1 :word (seq "a")}
    even-bs {:state 0 :word "aba"} {:state 1 :word dfa/e})
  (t/are [d s1 s2] (not (dfa/yields*? d s1 s2))
    even-bs {:state 0 :word "aba"} {:state 0 :word "a"}))

(t/deftest test-accepts?
  (t/are [d s] (dfa/accepts? d s)
    even-bs "abba"
    even-bs dfa/e
    even-bs "bb"
    even-bs "aa")
  (t/are [d s] (not (dfa/accepts? d s))
    even-bs "aba"
    even-bs "b"
    even-bs "b"))

(t/deftest test-invert
  (t/are [d1 d2] (= d1 d2)
    (dfa/invert even-bs)
    (dfa/map->dfa
     {:K #{0 1}
      :Sigma #{\a \b}
      :delta {0 {\a 0 \b 1}
              1 {\a 1 \b 0}}
      :s 0
      :F #{1}})))

(t/deftest test-transitions
  (t/are [d m] (= (dfa/transitions d) m)
    even-bs {0 #{0 1} 1 #{0 1}}))

(t/deftest test-has-transition
  (t/are [d s m] (= (dfa/has-transition d s) m)
    even-bs 1 {0 true 1 true}
    (dfa/map->dfa
     {:K #{0 1}
      :Sigma #{\a}
      :delta {0 {\a 0}
              1 {\a 1}}
      :s 0
      :F #{0}}) 1 {0 false 1 true}))

(def to-minimize
  (dfa/map->dfa
   {:K #{1 2 3 4 5 6 7 8}
    :Sigma #{\a \b}
    :delta {1 {\a 2 \b 4}
            2 {\a 5 \b 3}
            3 {\a 2 \b 6}
            4 {\a 1 \b 5}
            5 {\a 5 \b 5}
            6 {\a 3 \b 5}
            7 {\a 6 \b 8}
            8 {\a 7 \b 3}}
    :s 1
    :F #{1 3 7}}))

(def minimized
  (dfa/map->dfa
   {:K #{#{1 3}
         #{4 6}
         #{2}
         #{5}}
    :Sigma #{\a \b}
    :delta {#{1 3} {\a #{2} \b #{4 6}}
            #{4 6} {\a #{1 3} \b #{5}}
            #{2} {\a #{5} \b #{1 3}}
            #{5} {\a #{5} \b #{5}}}
    :s #{1 3}
    :F #{#{1 3}}}))

(t/deftest test-reachable
  (t/are [d s] (= (dfa/reachable d) s)
    simplest #{0}
    even-bs #{0 1}
    (dfa/map->dfa
     {:K #{0 1}
      :Sigma #{\a}
      :delta {0 {\a 0}
              1 {\a 1}}
      :s 0
      :F #{0}}) #{0}
    to-minimize #{1 2 3 4 5 6}))

(t/deftest test-unreachable
  (t/are [d s] (= (dfa/unreachable d) s)
    simplest #{}
    even-bs #{}
    (dfa/map->dfa
     {:K #{0 1}
      :Sigma #{\a}
      :delta {0 {\a 0}
              1 {\a 1}}
      :s 0
      :F #{0}}) #{1}
    to-minimize #{7 8}))

(t/deftest L-empty?
  (t/are [d] (dfa/L-empty? d)
    (dfa/map->dfa
     {:K #{0}
      :Sigma #{\a}
      :delta {0 {\a 0}}
      :s 0
      :F #{}})
    (dfa/map->dfa
     {:K #{0 1}
      :Sigma #{\a}
      :delta {0 {\a 0}
              1 {\a 1}}
      :s 0
      :F #{1}}))
  (t/are [d] (not (dfa/L-empty? d))
    simplest
    even-bs
    to-minimize
    minimized))

(t/deftest test-delete-unreachable
  (t/are [d1 d2] (= (dfa/delete-unreachable d1) d2)
    simplest simplest
    even-bs even-bs
    (dfa/map->dfa
     {:K #{0 1}
      :Sigma #{\a}
      :delta {0 {\a 0}
              1 {\a 1}}
      :s 0
      :F #{0 1}})
    (dfa/map->dfa
     {:K #{0}
      :Sigma #{\a}
      :delta {0 {\a 0}}
      :s 0
      :F #{0}})))

(def ex1 (dfa/dfa
          #{1 2 3 4 5}
          #{:a :b}
          {1 {:a 3 :b 2}
           2 {:a 4 :b 1}
           3 {:a 5 :b 4}
           4 {:a 4 :b 4}
           5 {:a 3 :b 2}}
          1
          #{1 5}))

(def ex1m (dfa/dfa
           #{#{1 5} #{2} #{3} #{4}}
           #{:a :b}
           {#{1 5} {:a #{3} :b #{2}}
            #{2} {:a #{4} :b #{1 5}}
            #{3} {:a #{1 5} :b #{4}}
            #{4} {:a #{4} :b #{4}}}
           #{1 5}
           #{#{1 5}}))

(def ex2 (dfa/dfa
          #{1 2 3 4 5 6}
          #{:a :b}
          {1 {:a 2 :b 3}
           2 {:a 2 :b 4}
           3 {:a 3 :b 3}
           4 {:a 6 :b 3}
           5 {:a 5 :b 3}
           6 {:a 5 :b 4}}
          1
          #{1 2 4 5 6}))

(def ex2m (dfa/dfa
           ;; same but as singletons
           #{#{1} #{2} #{3} #{4} #{5} #{6}}
           #{:a :b}
           {#{1} {:a #{2} :b #{3}}
            #{2} {:a #{2} :b #{4}}
            #{3} {:a #{3} :b #{3}}
            #{4} {:a #{6} :b #{3}}
            #{5} {:a #{5} :b #{3}}
            #{6} {:a #{5} :b #{4}}}
           #{1}
           #{#{1} #{2} #{4} #{5} #{6}}))

(t/deftest test-equivalent-states
  (t/are [d e] (= (dfa/equivalent-states d) e)
    (dfa/delete-unreachable to-minimize) #{#{1 3} #{4 6} #{2} #{5}}
    ex1 #{#{1 5} #{2} #{3} #{4}}))

(t/deftest test-minimize
  (t/are [d1 d2] (= (dfa/minimize d1) d2)
    to-minimize minimized
    ex1 ex1m
    ex2 ex2m))
