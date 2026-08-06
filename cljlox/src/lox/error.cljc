(ns lox.error
  (:require [lox.token :refer [TokenSchema]]))

(def ScannerErrorSchema
  [:map
   [:type [:= :scanner-error]]
   [:line :int]
   [:lexeme :string]
   [:message :string]])

(def ParserErrorSchema
  [:map
   [:type [:= :parser-error]]
   [:line :int]
   [:lexeme :string]
   [:token-type :keyword]
   [:message :string]])

(def AnalyserErrorSchema
  [:map
   [:type [:= :analyser-error]]
   [:token TokenSchema]
   [:message :string]])

(def ResolverErrorSchema
  [:map
   [:type [:= :resolver-error]]
   [:token TokenSchema]
   [:message :string]])

(def EvaluatorErrorSchema
  [:map
   [:type [:= :evaluator-error]]
   [:token TokenSchema]
   [:message :string]])



(defmulti format-error
  {:malli/schema
   [:=>
    [:cat [:or ScannerErrorSchema ParserErrorSchema AnalyserErrorSchema ResolverErrorSchema EvaluatorErrorSchema]]
    :string]}
  :type)

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

;; Note: we are using structural typing (e.g. `[:map [:errors [:sequential ParserErrorSchema]]]`) rather than requiring
;; the full `ParserStateSchema` etc. This enforces strict typing while avoiding circular dependencies. We could avoid
;; this by making a dedicated schemas file, but I think it's more sensible to co-locate schemas. We may revisit this.

(defn scanner-error
  {:malli/schema [:=>
                  [:cat [:map [:errors [:sequential ScannerErrorSchema]]] :int :string :string]
                  [:tuple :nil [:map [:errors [:sequential ScannerErrorSchema]]]]]}
  [state line lexeme message]
  [nil (update state :errors (fnil conj []) {:type :scanner-error, :line line, :lexeme lexeme, :message message})])

(defn parser-error
  {:malli/schema [:=>
                  [:cat [:map [:errors [:sequential ParserErrorSchema]]] TokenSchema :string]
                  [:tuple :nil [:map [:errors [:sequential ParserErrorSchema]]]]]}
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
  {:malli/schema [:=>
                  [:cat [:map [:errors [:sequential AnalyserErrorSchema]]] TokenSchema :string]
                  [:tuple :nil [:map [:errors [:sequential AnalyserErrorSchema]]]]]}
  [state token message]
  [nil (update state :errors (fnil conj []) {:type :analyser-error, :token token, :message message})])

(defn resolver-error
  {:malli/schema [:=>
                  [:cat [:map [:errors [:sequential ResolverErrorSchema]]] TokenSchema :string]
                  [:tuple :nil [:map [:errors [:sequential ResolverErrorSchema]]]]]}
  [state token message]
  [nil (update state :errors (fnil conj []) {:type :resolver-error, :token token, :message message})])

(defn evaluator-error
  {:malli/schema [:=> [:cat TokenSchema :string] :nil]}
  [token message]
  (throw (ex-info message
                  {:type  :evaluator-error,
                   :token token})))
