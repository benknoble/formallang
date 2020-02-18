(ns formallang.functional)

(defn mmap
  "Returns a map {k (f v)} for all (k v) pairs in m."
  [f m]
  ;; equivalently, (reduce (fn [acc cur] (update acc cur f)) m (keys m))
  (into (empty m) (for [[k v] m] [k (f v)])))
