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

(defn push-scope [env] (assoc env :locals (conj (:locals env) {})))

(defn pop-scope [env] (assoc env :locals (pop (:locals env))))

(defn with-globals [env caller-env] (assoc env :globals (:globals caller-env)))

(defn has-local? [env lexeme] (let [locals (:locals env)] (if (empty? locals) false (contains? (peek locals) lexeme))))

(defn get-local-address [env lexeme] (get (peek (:locals env)) lexeme))

(defn bind-local-address
  [env lexeme address]
  (let [locals (:locals env)]
    (if (empty? locals)
      (assoc-in env [:globals lexeme] address)
      (let [new-local-scope (assoc (peek locals) lexeme address)]
        (assoc env :locals (conj (pop locals) new-local-scope))))))

(defn define
  {:malli/schema [:=> [:cat EnvSchema TokenSchema memory/ValueSchema] EnvSchema]}
  [env name-token value]
  (let [lexeme (:lexeme name-token)
        address (memory/alloc! value)]
    (bind-local-address env lexeme address)))
