(ns lox.macros)

(defmacro with-scope
  [[state-binding initial-state] & body]
  `(let [~state-binding (update ~initial-state :scopes #(cons {} %))
         [result# state-after-body#] (do ~@body)]
     [result# (update state-after-body# :scopes rest)]))
