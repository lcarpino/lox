(ns lox.ast
  (:require [lox.token :refer [TokenSchema]]))

(def ExprSchema
  [:schema
   {:registry
    {::expr
     [:multi
      {:dispatch :type}
      [:assign
       [:map
        [:type [:= :assign]]
        [:name TokenSchema]
        [:value [:ref ::expr]]
        [:depth {:optional true} :int]]]
      [:binary
       [:map
        [:type [:= :binary]]
        [:op TokenSchema]
        [:left [:ref ::expr]]
        [:right [:ref ::expr]]]]
      [:call
       [:map
        [:type [:= :call]]
        [:callee [:ref ::expr]]
        [:paren TokenSchema]
        [:arguments [:sequential [:ref ::expr]]]]]
      [:get
       [:map
        [:type [:= :get]]
        [:object [:ref ::expr]]
        [:name TokenSchema]]]
      [:grouping
       [:map
        [:type [:= :grouping]]
        [:expression [:ref ::expr]]]]
      [:literal
       [:map
        [:type [:= :literal]]
        [:value :any]]]
      [:logical
       [:map
        [:type [:= :logical]]
        [:op TokenSchema]
        [:left [:ref ::expr]]
        [:right [:ref ::expr]]]]
      [:set
       [:map
        [:type [:= :set]]
        [:object [:ref ::expr]]
        [:name TokenSchema]
        [:value [:ref ::expr]]]]
      [:super
       [:map
        [:type [:= :super]]
        [:keyword TokenSchema]
        [:method TokenSchema]
        [:depth {:optional true} :int]]]
      [:this
       [:map
        [:type [:= :this]]
        [:keyword TokenSchema]]]
      [:unary
       [:map
        [:type [:= :unary]]
        [:op TokenSchema]
        [:right [:ref ::expr]]]]
      [:variable
       [:map
        [:type [:= :variable]]
        [:name TokenSchema]
        [:depth {:optional true} :int]]]]}}
   ::expr])

(def StmtSchema
  [:schema
   {:registry
    {::stmt
     [:multi
      {:dispatch :type}
      [:block
       [:map
        [:type [:= :block]]
        [:statements [:sequential [:ref ::stmt]]]]]
      [:class
       [:map
        [:type [:= :class]]
        [:name TokenSchema]
        [:superclass [:maybe ExprSchema]]
        [:methods [:sequential [:ref ::stmt]]]]]
      [:expr
       [:map
        [:type [:= :expr]]
        [:expression ExprSchema]]]
      [:function
       [:map
        [:type [:= :function]]
        [:name TokenSchema]
        [:params [:sequential TokenSchema]]
        [:body [:sequential [:ref ::stmt]]]]]
      [:if
       [:map
        [:type [:= :if]]
        [:condition ExprSchema]
        [:then-branch [:ref ::stmt]]
        [:else-branch [:maybe [:ref ::stmt]]]]]
      [:print
       [:map
        [:type [:= :print]]
        [:expression ExprSchema]]]
      [:return
       [:map
        [:type [:= :return]]
        [:keyword TokenSchema]
        [:value [:maybe ExprSchema]]]]
      [:var-stmt
       [:map
        [:type [:= :var-stmt]]
        [:name TokenSchema]
        [:initialiser [:maybe ExprSchema]]]]
      [:while
       [:map
        [:type [:= :while]]
        [:condition ExprSchema]
        [:body [:ref ::stmt]]]]]}}
   ::stmt])
