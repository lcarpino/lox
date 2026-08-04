(ns lox.error)

(defmulti format-error :type)

(defmethod format-error :scanner-error [{:keys [line message]}] (str "[line " line "] Error: " message))

(defmethod format-error :parser-error
  [{:keys [line lexeme token-type message]}]
  (let [where (cond (= token-type :eof) " at end"
                    lexeme (str " at '" lexeme "'")
                    :else "")]
    (str "[line " line "] Error" where ": " message)))

(defmethod format-error :analyser-error
  [{:keys [token message]}]
  (str "[line " (:line token) "] Error at '" (:lexeme token) "': " message))

(defmethod format-error :resolver-error
  [{:keys [token message]}]
  (str "[line " (:line token) "] Error at '" (:lexeme token) "': " message))

(defn parser-error
  [state token message]
  (let [err {:type       :parser-error,
             :line       (:line token),
             :lexeme     (:lexeme token),
             :token-type (:type token),
             :message    message}]
    [nil
     (-> state
         (update :errors (fnil conj []) err)
         (assoc :mode :recovering))]))

(defn analyser-error
  [state token message]
  [nil (update state :errors (fnil conj []) {:type :analyser-error, :token token, :message message})])

(defn resolver-error
  [state token message]
  [nil (update state :errors (fnil conj []) {:type :resolver-error, :token token, :message message})])

(defn evaluator-error
  [token message]
  (throw (ex-info message
                  {:type  :evaluator-error,
                   :token token})))
