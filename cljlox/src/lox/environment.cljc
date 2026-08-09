(ns lox.environment
  (:require [lox.memory :as memory]
            [lox.token :refer [TokenSchema]]))

(def EnvSchema
  [:map
   [:locals [:sequential [:map-of :string memory/AddressSchema]]]
   [:globals [:map-of :string memory/AddressSchema]]])

(defn resolve-address
  {:malli/schema [:=> [:cat EnvSchema TokenSchema [:maybe :int]] memory/AddressSchema]}
  [env name-token depth]
  (let [lexeme (:lexeme name-token)
        address
        (if depth (get (nth (:locals env) (- (count (:locals env)) 1 depth)) lexeme) (get (:globals env) lexeme))]
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
        locals (:locals env)]
    (if (empty? locals)
      (assoc-in env [:globals lexeme] address)
      (let [new-local-scope (assoc (peek locals) lexeme address)]
        (assoc env :locals (conj (pop locals) new-local-scope))))))
