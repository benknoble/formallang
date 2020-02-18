(ns formallang.dfa
  (:require
   [clojure
    [set :as set]]
   [formallang
    [fix :as fix]
    [functional :as func]]))

(defrecord DFA [K Sigma delta s F])

(defn dfa
  "custom (validating) constructor"
  [K Sigma delta s F]
  {:pre [;; delta: K → Sigma → K
         (and
          (= K (set (keys delta))) ; K →
          (every? #(= Sigma (set (keys %))) (vals delta)) ; Sigma →
          (set/subset? (->> delta vals (mapcat vals) set) K)) ; K
         ;; s ∈ K
         (contains? K s)
         ;; F ⊆ K
         (set/subset? F K)]}
  (->DFA K Sigma delta s F))

(defn map->dfa
  "custom (validating) map constructor"
  [{:keys [K Sigma delta s F]}]
  (dfa K Sigma delta s F))

(defrecord Conf [state word])

(def e "empty string" ())

(defn step1
  "Runs the DFA for one step by applying the transition function delta.

  Returns a Conf such that
  - state nil if inputs were invalid
  - word e if word has been exhausted
  - conf →(DFA) Conf otherwise."
  [dfa conf]
  (let [d (:delta dfa)
        {:keys [state word]} conf
        [hd & tl] word
        step' (get-in d [state hd])]
    (if (nil? step')
      {:state nil :word nil}
      {:state step' :word (if (nil? tl) e tl)})))

(defn finished?
  "True iff the DFA failed or exhausted its input."
  [conf]
  (let [{:keys [state word]} conf]
    (or
     (= nil state) ; failed
     (= e word)))) ; processed whole word

(defn step-until
  "Iterates a DFA until the finished? or pred holds true for the current
  configuration."
  [pred dfa conf]
  (if (or (finished? conf)
          (pred conf))
    conf
    (recur pred dfa (step1 dfa conf))))

(def step*
  "Iterates a DFA until it is finished."
  (partial step-until finished?))

(defn accepts-state?
  "True iff state ∈ F."
  [dfa state]
  (contains? (:F dfa) state))

(defn accepts-conf?
  "True iff word is e and state ∈ F."
  [dfa conf]
  (let [{:keys [state word]} conf]
    (and
     (= e word)
     (accepts-state? dfa state))))

(defn yields1?
  "True iff Conf1 →(DFA) Conf2."
  [dfa conf1 conf2]
  (= (step1 dfa conf1) conf2))

(defn yields*?
  "True iff Conf1 →*(DFA) Conf2."
  [dfa conf1 conf2]
  (let [final (step-until (partial = conf2) dfa conf1)]
    (= final conf2)))

(defn accepts?
  "True iff ∃q ∈ F such that (s,input) →*(DFA) (q,e)."
  [dfa input]
  (accepts-conf?
   dfa
   (step* dfa {:state (:s dfa)
               :word input})))

(defn invert
  "Returns the complement of a DFA."
  [dfa]
  (let [{:keys [F K]} dfa
        f-comp (set/difference K F)]
    (map->dfa (assoc dfa :F f-comp))))

(defn transitions
  "Returns a map: K → P(K), where (k,v) ∈ map ⇔ state k has a transition to all
  states in v."
  [dfa]
  (func/mmap (comp set vals) (:delta dfa)))

(defn has-transition
  "Returns a map: K -> Boolean, where (k,v) ∈ map ⇔ k has a transition to 'to'
  if v."
  [dfa to]
  (func/mmap #(contains? % to) (transitions dfa)))

(defn reachable
  "Returns the set of reachable states of a DFA."
  [dfa]
  (letfn [(step [prev d]
            (apply set/union
                   prev
                   (for [p prev
                         a (:Sigma d)]
                     (let [q (get-in (:delta dfa) [p a])]
                       #{q}))))]
    ((fix/fix step #{(:s dfa)}) dfa)))

(defn unreachable
  "Returns the set of unreachable states of a DFA."
  [dfa]
  (set/difference (:K dfa) (reachable dfa)))

(defn L-empty?
  "True iff the language accepted by a DFA is ∅."
  [dfa]
  (not-any? (:F dfa) (reachable dfa)))

(defn delete-unreachable
  "Returns the DFA without its unreachable states."
  [dfa]
  (let [to-delete (unreachable dfa)
        K (set/difference (:K dfa) to-delete)
        F (set/difference (:F dfa) to-delete)
        ;; it is provable that, after deleting the unreachable nodes, there are
        ;; no nodes left that can transition to those nodes, so there are no
        ;; connections left to delete.
        ;;
        ;; Why?
        ;;
        ;; Because all the remaining nodes are reachable. If one had a
        ;; connection to a just-deleted (unreachable) node, it would have in
        ;; fact been reachable, a contradiction.
        d (apply dissoc (:delta dfa) to-delete)]
    (assoc dfa :K K :F F :delta d)))

(defn equivalent-states
  "Returns a set of sets, where each set is an equivalence class of states in
  K."
  [dfa]
  (let [{:keys [K F Sigma delta]} dfa]
    (letfn [(contains-pair [p q eqs]
              (some (partial set/subset? (set [p q])) eqs))
            (≡ [p q eqs]
              (and (contains-pair p q eqs)
                   (every?
                    (fn [a] (contains-pair
                             (get-in delta [p a])
                             (get-in delta [q a])
                             eqs))
                    Sigma)))
            (step [prev _]
              (->> K
                   (reduce (fn [acc p]
                             (let [ks (keys acc)
                                   classes (filter #(≡ p % prev) ks)]
                               (if-let [clas (first classes)]
                                 (update acc clas conj p)
                                 (merge acc {p #{p}}))))
                           {})
                   vals
                   set))]
      ((fix/fix step #{F (set/difference K F)}) dfa))))

(defn- eq-state-map
  "Returns a map: K → P(K) where (k, s) ∈ map means k is in the equivalence
  class s."
  [eqs]
  (apply merge (for [eq eqs
                     e eq]
                 {e eq})))

(defn merge-equivalent
  "Returns a DFA recognizing the same language but with equivalent states
  merged."
  [dfa]
  (let [{:keys [Sigma delta s F]} dfa
        eqs (equivalent-states dfa)
        eqm (eq-state-map eqs)
        K' eqs
        Sigma' Sigma
        delta' (apply merge (for [k K']
                              (let [q (first k)
                                    d (get delta q)
                                    d' (func/mmap (partial get eqm) d)]
                                {k d'})))
        s' (get eqm s)
        ;; equivalent (in theory) to (set (filter #(set/subset? % F) eqs))
        F' (set (for [f F] (get eqm f)))]
    (map->dfa {:K K' :Sigma Sigma' :delta delta' :s s' :F F'})))

(def minimize
  "Returns a DFA recognizing the same language with the minimum number of
  states."
  (comp merge-equivalent delete-unreachable))
