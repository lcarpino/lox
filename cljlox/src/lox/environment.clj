(ns lox.environment
  (:require [lox.memory :as memory]
            [lox.token :refer [TokenSchema]]))

(def EnvSchema [:sequential [:map-of :string memory/AddressSchema]])

(defn empty-env {:malli/schema [:=> [:cat] EnvSchema]} [] '({}))

(defn resolve-address
  {:malli/schema [:=> [:cat EnvSchema TokenSchema [:maybe :int]] memory/AddressSchema]}
  [env name-token depth]
  (let [lexeme (:lexeme name-token)
        address (if depth (get (nth env depth) lexeme) (get (last env) lexeme))]
    (if address
      address
      (throw (ex-info (str "Undefined variable '" lexeme "'.")
                      {:type  :evaluator-error,
                       :token name-token})))))

(defn define
  {:malli/schema [:=> [:cat EnvSchema TokenSchema memory/ValueSchema] EnvSchema]}
  [env name-token value]
  (let [lexeme (:lexeme name-token)
        address (memory/alloc! value)
        new-local-scope (assoc (first env) lexeme address)]
    (cons new-local-scope (rest env))))
