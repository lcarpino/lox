(ns lox.native
  (:require [lox.memory :as memory]
            [lox.environment :as environment]))

(def ^:private native-functions
  {"clock" {:type    :native-function,
            :arity   0,
            :call-fn (fn [_]
                       (/ (double #?(:clj (System/currentTimeMillis)
                                     :cljs (.getTime (js/Date.))))
                          1000.0))}
  })

(defn create-global-env
  {:malli/schema [:=> [:cat] environment/EnvSchema]}
  []
  (loop [funcs (seq native-functions)
         env {}]
    (if (empty? funcs)
      {:locals [], :globals env}
      (let [[name-str func-map] (first funcs)
            address (memory/alloc! func-map)]
        (recur (rest funcs) (assoc env name-str address))))))
