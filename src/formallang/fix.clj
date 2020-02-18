(ns formallang.fix)

(defn fix
  "Returns a function which iterates a step function fi to its fix-point,
  starting from the initial state f0.

  The step function must have arity n >= 1. Then (fix step f0) has arity n-1."
  [fi f0]
  (fn [& args]
    (letfn [(go
              [prev cur]
              (if (= prev cur)
                cur
                (recur cur (apply fi cur args))))]
      (go f0 (apply fi f0 args)))))
