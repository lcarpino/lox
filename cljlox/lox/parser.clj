(ns lox.parser
  (:require [lox.scanner :as scanner]
            [lox.ast :as ast]))

(def ParserStateSchema
  [:map
   [:tokens [:sequential scanner/TokenSchema]]])

(def ParserFnSchema [:=> [:cat ParserStateSchema] [:tuple ast/ExprSchema ParserStateSchema]])

(def ParserOutputSchema [:sequential ast/StmtSchema])

(declare parse-expression)
(declare parse-statement)

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
  {:malli/schema [:=> [:cat [:set :keyword] ParserFnSchema] ParserFnSchema]}
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

(defn parse-assignment
  {:malli/schema [:=> [:cat ParserStateSchema [:tuple ast/ExprSchema ParserStateSchema]]]}
  [state]
  (let [[left-expr state-after-left] (parse-equality state)
        token (first (:tokens state-after-left))]
    (if (and token (= (:type token) :equal))
      (let [state-after-equal (assoc state-after-left :tokens (rest (:tokens state-after-left)))
            [value-expr state-after-value] (parse-assignment state-after-equal)]
        (if (= (:type left-expr) :variable)
          [{:type :assign, :name (:name left-expr), :value value-expr} state-after-value]
          (throw (ex-info "Invalid assignment target." {:line (:line token)}))))
      [left-expr state-after-left])))

(defn parse-expression
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/ExprSchema ParserStateSchema]]}
  [state]
  (parse-assignment state))

(defn- parse-block
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (loop [statements []
         current-state state]
    (let [token (first (:tokens current-state))]
      (cond (or (nil? token) (= (:type token) :eof)) (throw (ex-info "Expect '}' after block." {:line (:line token)}))
            (= (:type token) :rbrace) [{:type :block, :statements statements}
                                       (assoc current-state :tokens (rest (:tokens current-state)))]
            :else (let [[statement next-state] (parse-statement current-state)]
                    (recur (conj statements statement) next-state))))))

(defn- parse-print-statement
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [state-after-print (assoc state :tokens (rest (:tokens state)))
        [expr state-after-expr] (parse-expression state-after-print)
        semicolon-token (first (:tokens state-after-expr))]
    (if (= (:type semicolon-token) :semicolon)
      [{:type :print, :expression expr}
       (assoc state-after-expr :tokens (rest (:tokens state-after-expr)))]
      (throw (ex-info "Expect ';' after value." {:line (:line semicolon-token)})))))

(defn- parse-expr-statement
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [[expr state-after-expr] (parse-expression state)
        semicolon-token (first (:tokens state-after-expr))]
    (if (= (:type semicolon-token) :semicolon)
      [{:type :expr, :expression expr}
       (assoc state-after-expr :tokens (rest (:tokens state-after-expr)))]
      (throw (ex-info "Expect ';' after expression." {:line (:line semicolon-token)})))))

(defn- parse-var-statement
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [state-after-var (assoc state :tokens (rest (:tokens state)))
        name-token (first (:tokens state-after-var))]
    (if (= (:type name-token) :identifier)
      (let [state-after-name (assoc state-after-var :tokens (rest (:tokens state-after-var)))
            next-token (first (:tokens state-after-name))]
        (cond (= (:type next-token) :equal)
              (let [state-after-equal (assoc state-after-name :tokens (rest (:tokens state-after-name)))
                    [expr state-after-expr] (parse-expression state-after-equal)
                    semi-token (first (:tokens state-after-expr))]
                (if (= (:type semi-token) :semicolon)
                  [{:type :var-stmt, :name name-token, :initialiser expr}
                   (assoc state-after-expr :tokens (rest (:tokens state-after-expr)))]
                  (throw (ex-info "Expect ';' after variable declaration." {:line (:line semi-token)}))))
              (= (:type next-token) :semicolon) [{:type :var-stmt, :name name-token, :initialiser nil}
                                                 (assoc state-after-name :tokens (rest (:tokens state-after-name)))]
              :else (throw (ex-info "Expect ';' or '=' after variable name." {:line (:line next-token)}))))
      (throw (ex-info "Expect variable name" {:line (:line name-token)})))))

(defn parse-statement
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [token (first (:tokens state))]
    (cond (= (:type token) :print) (parse-print-statement state)
          (= (:type token) :var) (parse-var-statement state)
          (= (:type token) :lbrace) (parse-block (assoc state :tokens (rest (:tokens state))))
          :else (parse-expr-statement state))))

(defn parse
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ParserOutputSchema ParserStateSchema]]}
  [initial-state]
  (loop [statements []
         state initial-state]
    (let [token (first (:tokens state))]
      (if (or (nil? token) (= (:type token) :eof))
        [statements state]
        (let [[stmt next-state] (parse-statement state)] (recur (conj statements stmt) next-state))))))
