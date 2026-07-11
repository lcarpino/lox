(ns lox.parser
  (:require [lox.scanner :as scanner]
            [lox.ast :as ast]))

(def ParserStateSchema [:map [:tokens [:sequential scanner/TokenSchema]]])

(declare parse-expression)

(defn parse-primary
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/ExprSchema ParserStateSchema]]}
  [state]
  (let [[token & rest-tokens] (:tokens state)
        next-state (assoc state :tokens rest-tokens)]
    (cond
      ;; --- literals ---
      (= (:type token) :false) [{:type :literal, :value false} next-state]
      (= (:type token) :true) [{:type :literal, :value true} next-state]
      (= (:type token) :nil) [{:type :literal, :value nil} next-state]
      (#{:number :string} (:type token)) [{:type :literal, :value (:literal token)} next-state]
      ;; --- variables ---
      (= (:type token) :identifier) [{:type :variable, :name token} next-state]
      ;; --- grouping ---
      (= (:type token) :lparen) (let [[inner-expr state-after-expr] (parse-expression next-state)
                                      closing-token (first (:tokens state-after-expr))]
                                  (if (= (:type closing-token) :rparen)
                                    [{:type :grouping, :expression inner-expr}
                                     (assoc state-after-expr :tokens (rest (:tokens state-after-expr)))]
                                    (throw (ex-info "Expected ')' after expression." {:line (:line token)}))))
      ;; --- fallback ---
      :else (throw (ex-info "Expected expression." {:line (:line token)})))))


(defn parse-unary
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/ExprSchema ParserStateSchema]]}
  [state]
  (let [token (first (:tokens state))]
    (if (#{:bang :minus} (:type token))
      (let [state-after-op (assoc state :tokens (rest (:tokens state)))
            [right-expr final-state] (parse-unary state-after-op)]
        [{:type :unary, :op token, :right right-expr} final-state])
      (parse-primary state))))

(defn- make-binary-parser
  [operator-types next-parser-fn]
  (fn [initial-state]
    (let [[expr state-after-left] (next-parser-fn initial-state)]
      (loop [left-expr expr
             state state-after-left]
        (let [token (first (:tokens state))]
          (if (contains? operator-types (:type token))
            (let [state-after-op (assoc state :tokens (rest (:tokens state)))
                  [right-expr state-after-right] (next-parser-fn state-after-op)
                  new-binary-node {:type :binary, :op token, :left left-expr, :right right-expr}]
              (recur new-binary-node state-after-right))
            [left-expr state]))))))


(def parse-factor (make-binary-parser #{:slash :star} parse-unary))

(def parse-term (make-binary-parser #{:minus :plus} parse-factor))

(def parse-comparison (make-binary-parser #{:less :less-equal :greater :greater-equal} parse-term))

(def parse-equality (make-binary-parser #{:bang-equal :equal-equal} parse-comparison))

(defn parse-expression
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/ExprSchema ParserStateSchema]]}
  [state]
  (parse-equality state))
