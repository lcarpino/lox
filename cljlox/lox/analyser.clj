(ns lox.analyser
  (:require [lox.ast :as ast]))

(def ContextSchema
  [:map
   [:function-type [:enum :none :function :method :initialiser]]
   [:class-type [:enum :none :class :subclass]]])

(def initial-context
  {:function-type :none,
   :class-type    :none})

(defmulti analyse-expr
  ^:private {:malli/schema [:=> [:cat ContextSchema ast/ExprSchema] :any]}
  (fn [context expr] (:type expr)))

(defmulti analyse-stmt
  ^:private {:malli/schema [:=> [:cat ContextSchema ast/StmtSchema] :any]}
  (fn [context stmt] (:type stmt)))

(defn analyse [statements] (doseq [stmt statements] (analyse-stmt initial-context stmt)) statements)

(defmethod analyse-expr :assign [context expr] (analyse-expr context (:value expr)) nil)

(defmethod analyse-expr :binary
  [context expr]
  (analyse-expr context (:left expr))
  (analyse-expr context (:right expr))
  nil)

(defmethod analyse-expr :this
  [context expr]
  (when (= (:class-type context) :none) (throw (ex-info "Can't use 'this' outside of a class." {:token (:keyword expr)}))))

(defmethod analyse-expr :default [_ _] nil)

(defmethod analyse-stmt :block [context stmt] (doseq [s (:statements stmt)] (analyse-stmt context s)) nil)

(defmethod analyse-stmt :class
  [context stmt]
  (let [new-context (assoc context :class-type :class)]
    (doseq [m (:methods stmt)] (analyse-stmt (assoc new-context :function-type :method) m))
    nil))

(defmethod analyse-stmt :function
  [context stmt]
  (let [new-context (assoc context :function-type :function)]
    (doseq [s (:body stmt)] (analyse-stmt new-context s))
    nil))

(defmethod analyse-stmt :if
  [context stmt]
  (analyse-expr context (:condition stmt))
  (analyse-stmt context (:then-branch stmt))
  (when (:else-branch stmt) (analyse-stmt context (:else-branch stmt)))
  nil)

(defmethod analyse-stmt :return
  [context stmt]
  (when (= (:function-type context) :none)
    (throw (ex-info "Can't return from top-level code." {:token (:keyword stmt)})))
  (when (and (= (:function-type context) :initialiser) (:value stmt))
    (throw (ex-info "Can't return a value from an initialiser." {:token (:keyword stmt)})))
  (when (:value stmt) (analyse-expr context (:value stmt)))
  nil)

(defmethod analyse-stmt :default [_ _] nil)
