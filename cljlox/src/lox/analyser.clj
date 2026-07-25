(ns lox.analyser
  (:require [lox.ast :as ast]))

(def ContextSchema
  [:map
   [:function-type [:enum :none :function :method :initialiser]]
   [:class-type [:enum :none :class :subclass]]])

(def initial-context
  {:function-type :none,
   :class-type    :none})

(defn- analyser-error [token message] (throw (ex-info message {:type :analyser-error, :token token})))

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

(defmethod analyse-expr :get [context expr] (analyse-expr context (:object expr)) nil)

(defmethod analyse-expr :set
  [context expr]
  (analyse-expr context (:value expr))
  (analyse-expr context (:object expr))
  nil)

(defmethod analyse-expr :this
  [context expr]
  (when (= (:class-type context) :none) (analyser-error (:keyword expr) "Can't use 'this' outside of a class."))
  nil)

(defmethod analyse-expr :super
  [context expr]
  (cond (= (:class-type context) :none) (analyser-error (:keyword expr) "Can't use 'super' outside of a class.")
        (not= (:class-type context) :subclass) (analyser-error (:keyword expr)
                                                               "Can't use 'super' in a class with no superclass."))
  nil)

(defmethod analyse-expr :default [_ _] nil)

(defmethod analyse-stmt :block [context stmt] (doseq [s (:statements stmt)] (analyse-stmt context s)) nil)

(defmethod analyse-stmt :class
  [context stmt]
  (let [has-superclass? (:superclass stmt)
        new-context (assoc context :class-type (if has-superclass? :subclass :class))]
    (when has-superclass? (analyse-expr context (:superclass stmt)))
    (doseq [m (:methods stmt)]
      (let [func-type (if (= "init" (:lexeme (:name m))) :initialiser :method)]
        (analyse-stmt (assoc new-context :function-type func-type) m)))
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
  (when (= (:function-type context) :none) (analyser-error (:keyword stmt) "Can't return from top-level code."))
  (when (and (= (:function-type context) :initialiser) (:value stmt))
    (analyser-error (:keyword stmt) "Can't return a value from an initializer."))
  (when (:value stmt) (analyse-expr context (:value stmt)))
  nil)

(defmethod analyse-stmt :default [_ _] nil)
