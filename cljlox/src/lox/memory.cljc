(ns lox.memory)

(def AddressSchema :int)

(def ValueSchema [:maybe :any])

(defn alloc
  {:malli/schema
   [:=> [:cat [:vector ValueSchema] ValueSchema] [:map [:address AddressSchema] [:mem [:vector ValueSchema]]]]}
  [mem value]
  (let [address (count mem)] {:address address, :mem (conj mem value)}))

(defn read-store
  {:malli/schema [:=> [:cat [:vector ValueSchema] AddressSchema] ValueSchema]}
  [mem address]
  (nth mem address))

(defn write-store
  {:malli/schema [:=> [:cat [:vector ValueSchema] AddressSchema ValueSchema] [:vector ValueSchema]]}
  [mem address new-value]
  (assoc mem address new-value))

(defn update-store
  {:malli/schema [:=> [:cat [:vector ValueSchema] AddressSchema fn? [:* :any]] [:vector ValueSchema]]}
  [mem address f & args]
  (apply update mem address f args))
