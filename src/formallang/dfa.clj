(ns formallang.dfa
  (:require
    [clojure
     [set :as set]]))

(defrecord DFA [K Sigma delta s F])

(defn dfa
  "custom (validating) constructor"
  [K Sigma delta s F]
  {:pre [; delta: K → Sigma → K
         (= K
            (set (keys delta))
            (->> delta vals (mapcat vals) set))
         (= Sigma (->> delta vals (mapcat keys) set))
         ; s ∈ K
         (contains? K s)
         ; F ⊆ K
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
  (if
    (or
      (finished? conf)
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
