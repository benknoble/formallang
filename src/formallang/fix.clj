(ns formallang.fix)

(defn fix
  "Returns a function which iterates a step function fi to its fix-point,
   starting from the initial state f0."
  [fi f0]
  (fn [a]
    (letfn [(go [prev cur]
              (if (= prev cur)
                cur
                (recur cur (fi a cur))))]
      (go f0 (fi a f0)))))
