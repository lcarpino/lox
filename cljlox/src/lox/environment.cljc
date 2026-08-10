(ns lox.environment
  (:require [lox.memory :as memory]
            [lox.token :refer [TokenSchema]]))

(def EnvSchema
  [:map
   [:locals [:sequential [:map-of :string memory/AddressSchema]]]
   [:globals [:map-of :string memory/AddressSchema]]])

(defn resolve-address
  {:malli/schema [:=> [:cat EnvSchema TokenSchema [:maybe :int]] [:maybe memory/AddressSchema]]}
  [env name-token depth]
  (let [lexeme (:lexeme name-token)
        address
        (if depth (get (nth (:locals env) (- (count (:locals env)) 1 depth)) lexeme) (get (:globals env) lexeme))]
    address))

(defn push-scope {:malli/schema [:=> [:cat EnvSchema] EnvSchema]} [env] (assoc env :locals (conj (:locals env) {})))

(defn pop-scope {:malli/schema [:=> [:cat EnvSchema] EnvSchema]} [env] (assoc env :locals (pop (:locals env))))

(defn with-globals
  {:malli/schema [:=> [:cat EnvSchema EnvSchema] EnvSchema]}
  [env caller-env]
  (assoc env :globals (:globals caller-env)))

(defn has-local?
  {:malli/schema [:=> [:cat EnvSchema :string] :boolean]}
  [env lexeme]
  (let [locals (:locals env)] (if (empty? locals) false (contains? (peek locals) lexeme))))

(defn get-local-address
  {:malli/schema [:=> [:cat EnvSchema :string] [:maybe memory/AddressSchema]]}
  [env lexeme]
  (get (peek (:locals env)) lexeme))

(defn bind-local-address
  {:malli/schema [:=> [:cat EnvSchema :string memory/AddressSchema] EnvSchema]}
  [env lexeme address]
  (let [locals (:locals env)]
    (if (empty? locals)
      (assoc-in env [:globals lexeme] address)
      (let [new-local-scope (assoc (peek locals) lexeme address)]
        (assoc env :locals (conj (pop locals) new-local-scope))))))

(defn create-global {:malli/schema [:=> [:cat] EnvSchema]} [] {:locals [], :globals {}})
