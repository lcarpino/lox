(ns lox.ast
  (:require [lox.scanner :as scanner]))

(def ExprSchema
  [:schema
   {:registry
    {::expr
     [:multi
      {:dispatch :type}
      [:literal
       [:map
        [:type [:= :literal]]
        [:value :any]]]
      [:variable
       [:map
        [:type [:= :variable]]
        [:name scanner/TokenSchema]]]
      [:unary
       [:map
        [:type [:= :unary]]
        [:op scanner/TokenSchema]
        [:right [:ref ::expr]]]]
      [:binary
       [:map
        [:type [:= :binary]]
        [:op scanner/TokenSchema]
        [:left [:ref ::expr]]
        [:right [:ref ::expr]]]]
      [:grouping
       [:map
        [:type [:= :grouping]]
        [:expression [:ref ::expr]]]]
      [:assign
       [:map
        [:type [:= :assign]]
        [:name scanner/TokenSchema]
        [:value [:ref ::expr]]]]]}}
   ::expr])

(def StmtSchema
  [:schema
   {:registry
    {::stmt
     [:multi
      {:dispatch :type}
      [:print
       [:map
        [:type [:= :print]]
        [:expression ExprSchema]]]
      [:expr
       [:map
        [:type [:= :expr]]
        [:expression ExprSchema]]]
      [:var
       [:map
        [:type [:= :var]]
        [:initialiser [:maybe ExprSchema]]]]]}}
   ::stmt])
