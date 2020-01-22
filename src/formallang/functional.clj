(ns formallang.functional)

(defn mmap
  "Returns a map {k (f v)} for all (k v) pairs in m."
  [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))
