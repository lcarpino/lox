(ns lox.memory)

(def AddressSchema :int)

(def ValueSchema [:maybe [:or :number :string :boolean]])

(def store (atom []))

(defn empty-store! {:malli/schema [:=> [:cat] :any]} [] (reset! store []))

(defn alloc!
  {:malli/schema [:=> [:cat ValueSchema] AddressSchema]}
  [value]
  (let [address (count @store)]
    (swap! store conj value)
    address))

(defn read-store {:malli/schema [:=> [:cat AddressSchema] ValueSchema]} [address] (nth @store address))

(defn write-store!
  {:malli/schema [:=> [:cat AddressSchema ValueSchema] :any]}
  [address new-value]
  (swap! store assoc address new-value))
