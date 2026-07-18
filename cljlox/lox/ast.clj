(ns lox.ast
  (:require [lox.token :refer [TokenSchema]]))

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
        [:name TokenSchema]
        [:depth {:optional true} :int]]]
      [:unary
       [:map
        [:type [:= :unary]]
        [:op TokenSchema]
        [:right [:ref ::expr]]]]
      [:binary
       [:map
        [:type [:= :binary]]
        [:op TokenSchema]
        [:left [:ref ::expr]]
        [:right [:ref ::expr]]]]
      [:grouping
       [:map
        [:type [:= :grouping]]
        [:expression [:ref ::expr]]]]
      [:assign
       [:map
        [:type [:= :assign]]
        [:name TokenSchema]
        [:value [:ref ::expr]]
        [:depth {:optional true} :int]]]
      [:logical
       [:map
        [:type [:= :logical]]
        [:op TokenSchema]
        [:left [:ref ::expr]]
        [:right [:ref ::expr]]]]
      [:call
       [:map
        [:type [:= :call]]
        [:callee [:ref ::expr]]
        [:paren TokenSchema]
        [:arguments [:sequential [:ref ::expr]]]]]]}}
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
      [:var-stmt
       [:map
        [:type [:= :var-stmt]]
        [:name TokenSchema]
        [:initialiser [:maybe ExprSchema]]]]
      [:block
       [:map
        [:type [:= :block]]
        [:statements [:sequential [:ref ::stmt]]]]]
      [:if
       [:map
        [:type [:= :if]]
        [:condition ExprSchema]
        [:then-branch [:ref ::stmt]]
        [:else-branch [:maybe [:ref ::stmt]]]]]
      [:while
       [:map
        [:type [:= :while]]
        [:condition ExprSchema]
        [:body [:ref ::stmt]]]]
      [:function
       [:map
        [:type [:= :function]]
        [:name TokenSchema]
        [:params [:sequential TokenSchema]]
        [:body [:sequential [:ref ::stmt]]]]]
      [:return
       [:map
        [:type [:= :return]]
        [:keyword TokenSchema]
        [:value [:maybe ExprSchema]]]]]}}
   ::stmt])
