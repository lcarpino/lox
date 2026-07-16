(ns lox.memory)

(def store (atom []))

(defn empty-store! [] (reset! store []))

(defn alloc!
  [value]
  (let [address (count @store)]
    (swap! store conj value)
    address))

(defn read-store [address] (nth @store address))

(defn write-store! [address new-value] (swap! store assoc address new-value))
