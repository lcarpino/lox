(ns lox.error)

(defn parser-error
  [token message]
  (throw (ex-info message
                  {:type       :parser-error,
                   :line       (:line token),
                   :lexeme     (:lexeme token),
                   :token-type (:type token)})))

(defn analyser-error [token message] (throw (ex-info message {:type :analyser-error, :token token})))

(defn resolver-error
  [token message]
  (throw (ex-info message
                  {:type  :resolver-error,
                   :token token})))

(defn evaluator-error
  [token message]
  (throw (ex-info message
                  {:type  :evaluator-error,
                   :token token})))
