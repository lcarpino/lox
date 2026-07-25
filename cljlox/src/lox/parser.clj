(ns lox.parser
  (:require [lox.ast :as ast]
            [lox.token :refer [TokenSchema]]))

(def ParserStateSchema
  [:map
   [:tokens [:sequential TokenSchema]]])

(def ParserFnSchema [:=> [:cat ParserStateSchema] [:tuple ast/ExprSchema ParserStateSchema]])

(def ParserOutputSchema [:sequential ast/StmtSchema])

(declare parse-expression)
(declare parse-declaration)
(declare parse-statement)

(defn- parse-error
  [token message]
  (throw (ex-info message
                  {:type       :parser-error,
                   :line       (:line token),
                   :lexeme     (:lexeme token),
                   :token-type (:type token)})))

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
      ;; --- keywords ---
      (= (:type token) :this) [{:type :this, :keyword token} next-state]
      ;; --- super ---
      (= (:type token) :super) (let [s-after-super (assoc state :tokens (rest (:tokens state)))
                                     dot-token (first (:tokens s-after-super))]
                                 (when-not (= (:type dot-token) :dot)
                                   (parse-error (or dot-token token) "Expect '.' after 'super'."))
                                 (let [s-after-dot (assoc s-after-super :tokens (rest (:tokens s-after-super)))
                                       method-token (first (:tokens s-after-dot))]
                                   (when-not (= (:type method-token) :identifier)
                                     (parse-error (or method-token dot-token) "Expect superclass method name."))
                                   [{:type :super, :keyword token, :method method-token}
                                    (assoc s-after-dot :tokens (rest (:tokens s-after-dot)))]))
      ;; --- variables ---
      (= (:type token) :identifier) [{:type :variable, :name token} next-state]
      ;; --- grouping ---
      (= (:type token) :lparen) (let [[inner-expr state-after-expr] (parse-expression next-state)
                                      closing-token (first (:tokens state-after-expr))]
                                  (if (= (:type closing-token) :rparen)
                                    [{:type :grouping, :expression inner-expr}
                                     (assoc state-after-expr :tokens (rest (:tokens state-after-expr)))]
                                    (parse-error token "Expect ')' after expression.")))
      ;; --- fallback ---
      :else (parse-error token "Expect expression."))))

(defn parse-call
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/ExprSchema ParserStateSchema]]}
  [state]
  (let [[expr state-after-expr] (parse-primary state)]
    (loop [callee expr
           current-state state-after-expr]
      (let [token (first (:tokens current-state))]
        (cond (= (:type token) :lparen)
              (let [state-after-lparen (assoc current-state :tokens (rest (:tokens current-state)))
                    [args state-after-args]
                    (if (= (:type (first (:tokens state-after-lparen))) :rparen)
                      [[] state-after-lparen]
                      (loop [args []
                             s state-after-lparen]
                        (when (>= (count args) 255)
                          (parse-error (first (:tokens s)) "Can't have more than 255 arguments."))
                        (let [[arg s-after-arg] (parse-expression s)
                              args (conj args arg)
                              next-token (first (:tokens s-after-arg))]
                          (if (= (:type next-token) :comma)
                            (recur args (assoc s-after-arg :tokens (rest (:tokens s-after-arg))))
                            [args s-after-arg]))))
                    paren-token (first (:tokens state-after-args))]
                (when-not (= (:type paren-token) :rparen) (parse-error paren-token "Expect ')' after arguments."))
                (recur {:type :call, :callee callee, :paren paren-token, :arguments args}
                       (assoc state-after-args :tokens (rest (:tokens state-after-args)))))
              (= (:type token) :dot) (let [state-after-dot (assoc current-state :tokens (rest (:tokens current-state)))
                                           name-token (first (:tokens state-after-dot))]
                                       (when-not (= (:type name-token) :identifier)
                                         (parse-error token "Expect property name after '.'."))
                                       (recur {:type :get, :object callee, :name name-token}
                                              (assoc state-after-dot :tokens (rest (:tokens state-after-dot)))))
              :else [callee current-state])))))

(defn parse-unary
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/ExprSchema ParserStateSchema]]}
  [state]
  (let [token (first (:tokens state))]
    (if (#{:bang :minus} (:type token))
      (let [state-after-op (assoc state :tokens (rest (:tokens state)))
            [right-expr final-state] (parse-unary state-after-op)]
        [{:type :unary, :op token, :right right-expr} final-state])
      (parse-call state))))

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

(defn- make-logical-parser
  {:malli/schema [:=> [:cat [:set :keyword] ParserFnSchema] ParserFnSchema]}
  [operator-types next-parser-fn]
  (fn [initial-state]
    (let [[expr state-after-left] (next-parser-fn initial-state)]
      (loop [left-expr expr
             state state-after-left]
        (let [token (first (:tokens state))]
          (if (contains? operator-types (:type token))
            (let [state-after-op (assoc state :tokens (rest (:tokens state)))
                  [right-expr state-after-right] (next-parser-fn state-after-op)]
              (recur {:type :logical, :op token, :left left-expr, :right right-expr} state-after-right))
            [left-expr state]))))))

(def parse-factor (make-binary-parser #{:slash :star} parse-unary))
(def parse-term (make-binary-parser #{:minus :plus} parse-factor))
(def parse-comparison (make-binary-parser #{:less :less-equal :greater :greater-equal} parse-term))
(def parse-equality (make-binary-parser #{:bang-equal :equal-equal} parse-comparison))

(def parse-and (make-logical-parser #{:and} parse-equality))
(def parse-or (make-logical-parser #{:or} parse-and))

(defn parse-assignment
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/ExprSchema ParserStateSchema]]}
  [state]
  (let [[left-expr state-after-left] (parse-or state)
        token (first (:tokens state-after-left))]
    (if (and token (= (:type token) :equal))
      (let [state-after-equal (assoc state-after-left :tokens (rest (:tokens state-after-left)))
            [value-expr state-after-value] (parse-assignment state-after-equal)]
        (cond (= (:type left-expr) :variable) [{:type :assign, :name (:name left-expr), :value value-expr}
                                               state-after-value]
              (= (:type left-expr) :get)
              [{:type :set, :object (:object left-expr), :name (:name left-expr), :value value-expr} state-after-value]
              :else (parse-error token "Invalid assignment target.")))
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
      (cond (or (nil? token) (= (:type token) :eof)) (parse-error token "Expect '}' after block.")
            (= (:type token) :rbrace) [{:type :block, :statements statements}
                                       (assoc current-state :tokens (rest (:tokens current-state)))]
            :else (let [[statement next-state] (parse-declaration current-state)]
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
      (parse-error semicolon-token "Expect ';' after value."))))

(defn- parse-expr-statement
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [[expr state-after-expr] (parse-expression state)
        semicolon-token (first (:tokens state-after-expr))]
    (if (= (:type semicolon-token) :semicolon)
      [{:type :expr, :expression expr}
       (assoc state-after-expr :tokens (rest (:tokens state-after-expr)))]
      (parse-error semicolon-token "Expect ';' after expression."))))

(defn- parse-var-statement
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [state-after-var (assoc state :tokens (rest (:tokens state)))
        name-token (first (:tokens state-after-var))]
    (if (= (:type name-token) :identifier)
      (let [state-after-name (assoc state-after-var :tokens (rest (:tokens state-after-var)))
            next-token (first (:tokens state-after-name))]
        (cond (= (:type next-token) :equal) (let [state-after-equal
                                                  (assoc state-after-name :tokens (rest (:tokens state-after-name)))
                                                  [expr state-after-expr] (parse-expression state-after-equal)
                                                  semi-token (first (:tokens state-after-expr))]
                                              (if (= (:type semi-token) :semicolon)
                                                [{:type :var-stmt, :name name-token, :initialiser expr}
                                                 (assoc state-after-expr :tokens (rest (:tokens state-after-expr)))]
                                                (parse-error semi-token "Expect ';' after variable declaration.")))
              (= (:type next-token) :semicolon) [{:type :var-stmt, :name name-token, :initialiser nil}
                                                 (assoc state-after-name :tokens (rest (:tokens state-after-name)))]
              :else (parse-error next-token "Expect ';' or '=' after variable name.")))
      (parse-error name-token "Expect variable name."))))

(defn- parse-if-statement
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [state (assoc state :tokens (rest (:tokens state)))
        lparen (first (:tokens state))]
    (when-not (= (:type lparen) :lparen) (parse-error lparen "Expect '(' after 'if'."))
    (let [state (assoc state :tokens (rest (:tokens state)))
          [condition state] (parse-expression state)
          rparen (first (:tokens state))]
      (when-not (= (:type rparen) :rparen) (parse-error rparen "Expect ')' after 'if' condition."))
      (let [state (assoc state :tokens (rest (:tokens state)))
            [then-branch state] (parse-statement state)
            else-token (first (:tokens state))]
        (if (= (:type else-token) :else)
          (let [state (assoc state :tokens (rest (:tokens state)))
                [else-branch state] (parse-statement state)]
            [{:type :if, :condition condition, :then-branch then-branch, :else-branch else-branch} state])
          [{:type :if, :condition condition, :then-branch then-branch, :else-branch nil} state])))))

(defn- parse-while-statement
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [state (assoc state :tokens (rest (:tokens state)))
        lparen (first (:tokens state))]
    (when-not (= (:type lparen) :lparen) (parse-error lparen "Expect '(' after 'while'."))
    (let [state (assoc state :tokens (rest (:tokens state)))
          [condition state] (parse-expression state)
          rparen (first (:tokens state))]
      (when-not (= (:type rparen) :rparen) (parse-error rparen "Expect ')' after 'while' condition."))
      (let [state (assoc state :tokens (rest (:tokens state)))
            [body state] (parse-statement state)]
        [{:type :while, :condition condition, :body body} state]))))

(defn- parse-for-statement
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [state (assoc state :tokens (rest (:tokens state)))
        lparen (first (:tokens state))]
    (when-not (= (:type lparen) :lparen) (parse-error lparen "Expect '(' after 'for'."))
    (let [state (assoc state :tokens (rest (:tokens state)))
          init-token (first (:tokens state))
          [init-stmt state] (cond (= (:type init-token) :semicolon) [nil (assoc state :tokens (rest (:tokens state)))]
                                  (= (:type init-token) :var) (parse-var-statement state)
                                  :else (parse-expr-statement state))
          cond-token (first (:tokens state))
          [cond-expr state] (if (= (:type cond-token) :semicolon) [nil state] (parse-expression state))
          semi-token (first (:tokens state))]
      (when-not (= (:type semi-token) :semicolon) (parse-error semi-token "Expect ';' after loop condition."))
      (let [state (assoc state :tokens (rest (:tokens state)))
            inc-token (first (:tokens state))
            [inc-expr state] (if (= (:type inc-token) :rparen) [nil state] (parse-expression state))
            rparen (first (:tokens state))]
        (when-not (= (:type rparen) :rparen) (parse-error rparen "Expect ')' after for clauses."))
        (let [state (assoc state :tokens (rest (:tokens state)))
              [body state] (parse-statement state)
              body-with-inc (if inc-expr {:type :block, :statements [body {:type :expr, :expression inc-expr}]} body)
              loop-cond (if cond-expr cond-expr {:type :literal, :value true})
              while-stmt {:type :while, :condition loop-cond, :body body-with-inc}
              final-stmt (if init-stmt {:type :block, :statements [init-stmt while-stmt]} while-stmt)]
          [final-stmt state])))))

(defn- parse-function
  {:malli/schema [:=> [:cat ParserStateSchema :enum :function :method] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state kind]
  (let [state (if (= kind :function) (assoc state :tokens (rest (:tokens state))) state)
        name-token (first (:tokens state))]
    (when-not (= (:type name-token) :identifier) (parse-error name-token (str "Expect " (name kind) " name.")))
    (let [state (assoc state :tokens (rest (:tokens state)))
          lparen (first (:tokens state))]
      (when-not (= (:type lparen) :lparen) (parse-error lparen (str "Expect '(' after " (name kind) " name.")))
      (let [state (assoc state :tokens (rest (:tokens state)))
            [params state-after-params]
            (if (= (:type (first (:tokens state))) :rparen)
              [[] state]
              (loop [params []
                     s state]
                (when (>= (count params) 255) (parse-error (first (:tokens s)) "Can't have more than 255 parameters."))
                (let [param-token (first (:tokens s))]
                  (when-not (= (:type param-token) :identifier) (parse-error param-token "Expect parameter name."))
                  (let [params (conj params param-token)
                        s-after-param (assoc s :tokens (rest (:tokens s)))
                        next-token (first (:tokens s-after-param))]
                    (if (= (:type next-token) :comma)
                      (recur params (assoc s-after-param :tokens (rest (:tokens s-after-param))))
                      [params s-after-param])))))
            rparen (first (:tokens state-after-params))]
        (when-not (= (:type rparen) :rparen) (parse-error rparen "Expect ')' after parameters."))
        (let [state (assoc state-after-params :tokens (rest (:tokens state-after-params)))
              lbrace (first (:tokens state))]
          (when-not (= (:type lbrace) :lbrace) (parse-error lbrace (str "Expect '{' before " (name kind) " body.")))
          (let [state (assoc state :tokens (rest (:tokens state)))
                [block-stmt state] (parse-block state)]
            [{:type :function, :name name-token, :params params, :body (:statements block-stmt)} state]))))))

(defn- parse-return-statement
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [return-token (first (:tokens state))
        state (assoc state :tokens (rest (:tokens state)))
        next-token (first (:tokens state))]
    (if (= (:type next-token) :semicolon)
      [{:type :return, :keyword return-token, :value nil} (assoc state :tokens (rest (:tokens state)))]
      (let [[expr state-after-expr] (parse-expression state)
            semi-token (first (:tokens state-after-expr))]
        (when-not (= (:type semi-token) :semicolon) (parse-error semi-token "Expect ';' after return value."))
        [{:type :return, :keyword return-token, :value expr}
         (assoc state-after-expr :tokens (rest (:tokens state-after-expr)))]))))

(defn- parse-class-declaration
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [state (assoc state :tokens (rest (:tokens state)))
        name-token (first (:tokens state))]
    (when-not (= (:type name-token) :identifier) (parse-error name-token "Expect class name."))
    (let [state-after-name (assoc state :tokens (rest (:tokens state)))
          maybe-less (first (:tokens state-after-name))
          [superclass state-after-super] (if (= (:type maybe-less) :less)
                                           (let [state-after-less
                                                 (assoc state-after-name :tokens (rest (:tokens state-after-name)))
                                                 super-name (first (:tokens state-after-less))]
                                             (when-not (= (:type super-name) :identifier)
                                               (parse-error super-name "Expect superclass name."))
                                             [{:type :variable, :name super-name}
                                              (assoc state-after-less :tokens (rest (:tokens state-after-less)))])
                                           [nil state-after-name])
          lbrace (first (:tokens state-after-super))]
      (when-not (= (:type lbrace) :lbrace) (parse-error (or lbrace name-token) "Expect '{' before class body."))
      (loop [methods []
             current-state (assoc state-after-super :tokens (rest (:tokens state-after-super)))]
        (let [token (first (:tokens current-state))]
          (cond (or (nil? token) (= (:type token) :eof)) (parse-error token "Expect '}' after class body.")
                (= (:type token) :rbrace) [{:type :class, :name name-token, :superclass superclass, :methods methods}
                                           (assoc current-state :tokens (rest (:tokens current-state)))]
                :else (let [[method next-state] (parse-function current-state :method)]
                        (recur (conj methods method) next-state))))))))

(defn parse-declaration
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [token (first (:tokens state))]
    (cond (= (:type token) :class) (parse-class-declaration state)
          (= (:type token) :fun) (parse-function state :function)
          (= (:type token) :var) (parse-var-statement state)
          :else (parse-statement state))))

(defn parse-statement
  {:malli/schema [:=> [:cat ParserStateSchema] [:tuple ast/StmtSchema ParserStateSchema]]}
  [state]
  (let [token (first (:tokens state))]
    (cond (= (:type token) :print) (parse-print-statement state)
          (= (:type token) :if) (parse-if-statement state)
          (= (:type token) :while) (parse-while-statement state)
          (= (:type token) :for) (parse-for-statement state)
          (= (:type token) :return) (parse-return-statement state)
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
        (let [[stmt next-state] (parse-declaration state)] (recur (conj statements stmt) next-state))))))
