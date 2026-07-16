(ns lox.token)

(def TokenSchema
  [:map
   [:type :keyword]
   [:lexeme :string]
   [:line :int]
   [:literal {:optional true} :any]])
