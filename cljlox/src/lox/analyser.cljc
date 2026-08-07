(ns lox.analyser
  (:require [lox.ast :as ast]
            [lox.error :as error]))

(def AnalyserStateSchema
  [:map
   [:function-type [:enum :none :function :method :initialiser]]
   [:class-type [:enum :none :class :subclass]]
   [:errors [:sequential error/AnalyserErrorSchema]]])

(def initial-context
  {:function-type :none,
   :class-type    :none})

(defmulti analyse-expr
  ^:private {:malli/schema [:=> [:cat AnalyserStateSchema ast/ExprSchema] [:tuple :nil AnalyserStateSchema]]}
  (fn [_ expr] (:type expr)))

(defmulti analyse-stmt
  ^:private {:malli/schema [:=> [:cat AnalyserStateSchema ast/StmtSchema] [:tuple :nil AnalyserStateSchema]]}
  (fn [_ stmt] (:type stmt)))

(defn analyse
  {:malli/schema [:=> [:cat [:sequential ast/StmtSchema]] [:tuple [:sequential ast/StmtSchema] AnalyserStateSchema]]}
  [statements]
  (loop [stmts statements
         ctx (assoc initial-context :errors [])]
    (if (empty? stmts)
      [statements ctx]
      (let [[_ next-ctx] (analyse-stmt ctx (first stmts))] (recur (rest stmts) next-ctx)))))

(defmethod analyse-expr :assign [context expr] (let [[_ ctx] (analyse-expr context (:value expr))] [nil ctx]))

(defmethod analyse-expr :binary
  [context expr]
  (let [[_ ctx] (analyse-expr context (:left expr))
        [_ ctx] (analyse-expr ctx (:right expr))]
    [nil ctx]))

(defmethod analyse-expr :get [context expr] (let [[_ ctx] (analyse-expr context (:object expr))] [nil ctx]))

(defmethod analyse-expr :set
  [context expr]
  (let [[_ ctx] (analyse-expr context (:value expr))
        [_ ctx] (analyse-expr ctx (:object expr))]
    [nil ctx]))

(defmethod analyse-expr :this
  [context expr]
  (if (= (:class-type context) :none)
    (error/analyser-error context (:keyword expr) "Can't use 'this' outside of a class.")
    [nil context]))

(defmethod analyse-expr :super
  [context expr]
  (cond (= (:class-type context) :none)
        (error/analyser-error context (:keyword expr) "Can't use 'super' outside of a class.")
        (not= (:class-type context) :subclass)
        (error/analyser-error context (:keyword expr) "Can't use 'super' in a class with no superclass.")
        :else [nil context]))

(defmethod analyse-expr :default [context _] [nil context])

(defmethod analyse-stmt :block
  [context stmt]
  (loop [stmts (:statements stmt)
         ctx context]
    (if (empty? stmts) [nil ctx] (let [[_ next-ctx] (analyse-stmt ctx (first stmts))] (recur (rest stmts) next-ctx)))))

(defmethod analyse-stmt :class
  [context stmt]
  (let [has-superclass? (:superclass stmt)
        new-context (assoc context :class-type (if has-superclass? :subclass :class))
        [_ ctx] (if has-superclass? (analyse-expr context (:superclass stmt)) [nil context])]
    (loop [methods (:methods stmt)
           current-ctx ctx]
      (if (empty? methods)
        [nil current-ctx]
        (let [m (first methods)
              func-type (if (= "init" (:lexeme (:name m))) :initialiser :method)
              [_ ctx-after] (analyse-stmt (assoc new-context :errors (:errors current-ctx) :function-type func-type) m)]
          (recur (rest methods) (assoc current-ctx :errors (:errors ctx-after))))))))

(defmethod analyse-stmt :function
  [context stmt]
  (let [new-context (assoc context :function-type :function)]
    (loop [stmts (:body stmt)
           ctx new-context]
      (if (empty? stmts)
        [nil (assoc context :errors (:errors ctx))]
        (let [[_ next-ctx] (analyse-stmt ctx (first stmts))] (recur (rest stmts) next-ctx))))))

(defmethod analyse-stmt :if
  [context stmt]
  (let [[_ ctx] (analyse-expr context (:condition stmt))
        [_ ctx] (analyse-stmt ctx (:then-branch stmt))]
    (if (:else-branch stmt) (analyse-stmt ctx (:else-branch stmt)) [nil ctx])))

(defmethod analyse-stmt :return
  [context stmt]
  (let [[_ ctx] (if (= (:function-type context) :none)
                  (error/analyser-error context (:keyword stmt) "Can't return from top-level code.")
                  [nil context])
        [_ ctx] (if (and (= (:function-type context) :initialiser) (:value stmt))
                  (error/analyser-error ctx (:keyword stmt) "Can't return a value from an initializer.")
                  [nil ctx])]
    (if (:value stmt) (analyse-expr ctx (:value stmt)) [nil ctx])))

(defmethod analyse-stmt :default [context _] [nil context])
