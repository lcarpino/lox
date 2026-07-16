(ns lox.environment
  (:require [lox.memory :as memory]
            [lox.scanner :as scanner]))

(def EnvSchema [:sequential [:map-of :string memory/AddressSchema]])

(defn empty-env {:malli/schema [:=> [:cat] EnvSchema]} [] '({}))

(defn resolve-address
  {:malli/schema [:=> [:cat EnvSchema scanner/TokenSchema] memory/AddressSchema]}
  [env name-token]
  (let [lexeme (:lexeme name-token)
        address (some #(get % lexeme) env)]
    (if address address (throw (ex-info (str "Undefined variable '" lexeme "'.") {:token name-token})))))

(defn define
  {:malli/schema [:=> [:cat EnvSchema scanner/TokenSchema memory/ValueSchema] EnvSchema]}
  [env name-token value]
  (let [lexeme (:lexeme name-token)
        address (memory/alloc! value)
        new-local-scope (assoc (first env) lexeme address)]
    (cons new-local-scope (rest env))))

