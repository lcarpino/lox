(ns lox.native)

(def native-functions
  {"clock" {:type    :native-function,
            :arity   0,
            :call-fn (fn [_ mem] [(/ (double #?(:clj (System/currentTimeMillis)
                                                :cljs (.getTime (js/Date.))))
                                     1000.0)
                                  mem])}
  })
