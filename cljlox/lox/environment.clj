(ns lox.environment
  (:require [lox.memory :as memory]))

(defn empty-env [] '({}))

(defn resolve-address
  [env name-token]
  (let [lexeme (:lexeme name-token)
        address (some #(get % lexeme) env)]
    (if address address (throw (ex-info (str "Undefined variable '" lexeme "'.") {:token name-token})))))

(defn define
  [env name-token value]
  (let [lexeme (:lexeme name-token)
        address (memory/alloc! value)
        new-local-scope (assoc (first env) lexeme address)]
    (cons new-local-scope (rest env))))

