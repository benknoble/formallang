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
            :F #{0}}) #{0}))

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
            :F #{0}}) #{1}))

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
         even-bs))
