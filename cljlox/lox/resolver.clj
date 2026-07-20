(ns lox.resolver
  (:require [lox.ast :as ast]))

(def initial-state {:scopes '(), :function-type :none, :class-type :none})

(defmulti resolve-expr ^:private (fn [state expr] (:type expr)))

(defmulti resolve-stmt ^:private (fn [state stmt] (:type stmt)))

(defmacro with-scope
  [[state-binding initial-state] & body]
  `(let [~state-binding (update ~initial-state :scopes #(cons {} %))
         [result# state-after-body#] (do ~@body)]
     [result# (update state-after-body# :scopes rest)]))

(defn- declare-var
  [state lexeme]
  (if (empty? (:scopes state))
    state
    (let [current-scope (first (:scopes state))]
      (if (contains? current-scope lexeme)
        (throw (ex-info "Already a variable with this name in this scope." {:lexeme lexeme}))
        (update state :scopes #(cons (assoc (first %) lexeme false) (rest %)))))))

(defn- define-var
  [state lexeme]
  (if (empty? (:scopes state)) state (update state :scopes #(cons (assoc (first %) lexeme true) (rest %)))))

(defn- resolve-local
  [state node name-token]
  (let [lexeme (:lexeme name-token)
        scopes (:scopes state)]
    (loop [depth 0
           remaining-scopes scopes]
      (cond (empty? remaining-scopes) node
            (contains? (first remaining-scopes) lexeme) (assoc node :depth depth)
            :else (recur (inc depth) (rest remaining-scopes))))))

(defn- resolve-params
  [state params]
  (loop [current-state state
         remaining-params params]
    (if (empty? remaining-params)
      current-state
      (let [param-name (:lexeme (first remaining-params))
            state-declared (declare-var current-state param-name)
            state-defined (define-var state-declared param-name)]
        (recur state-defined (rest remaining-params))))))

(defn- resolve-statements
  [state statements]
  (loop [stmts statements
         current-state state
         resolved []]
    (if (empty? stmts)
      [resolved current-state]
      (let [[r-stmt next-state] (resolve-stmt current-state (first stmts))]
        (recur (rest stmts) next-state (conj resolved r-stmt))))))

(defn- resolve-function-body
  [state stmt func-type]
  (let [saved-func-type (:function-type state)]
    (with-scope [inner-state (assoc state :function-type func-type)]
                (let [state-with-params (resolve-params inner-state (:params stmt))
                      [resolved-body state-after-body] (resolve-statements state-with-params (:body stmt))]
                  [(assoc stmt :body resolved-body)
                   (assoc state-after-body :function-type saved-func-type)]))))

(defmethod resolve-expr :assign
  [state expr]
  (let [[resolved-val state-after-val] (resolve-expr state (:value expr))
        enriched-assign (assoc expr :value resolved-val)
        final-assign (resolve-local state-after-val enriched-assign (:name expr))]
    [final-assign state-after-val]))

(defmethod resolve-expr :binary
  [state expr]
  (let [[left state-left] (resolve-expr state (:left expr))
        [right state-right] (resolve-expr state-left (:right expr))]
    [(assoc expr :left left :right right) state-right]))

(defmethod resolve-expr :call
  [state expr]
  (let [[callee state-callee] (resolve-expr state (:callee expr))
        [args state-args] (loop [remaining-args (:arguments expr)
                                 current-state state-callee
                                 resolved-args []]
                            (if (empty? remaining-args)
                              [resolved-args current-state]
                              (let [[r-arg next-state] (resolve-expr current-state (first remaining-args))]
                                (recur (rest remaining-args) next-state (conj resolved-args r-arg)))))]
    [(assoc expr :callee callee :arguments args) state-args]))

(defmethod resolve-expr :get
  [state expr]
  (let [[resolved-obj state-obj] (resolve-expr state (:object expr))]
    [(assoc expr :object resolved-obj) state-obj]))

(defmethod resolve-expr :grouping
  [state expr]
  (let [[inner state-inner] (resolve-expr state (:expression expr))] [(assoc expr :expression inner) state-inner]))

(defmethod resolve-expr :logical
  [state expr]
  (let [[left state-left] (resolve-expr state (:left expr))
        [right state-right] (resolve-expr state-left (:right expr))]
    [(assoc expr :left left :right right) state-right]))

(defmethod resolve-expr :set
  [state expr]
  (let [[resolved-value state-val] (resolve-expr state (:value expr))
        [resolved-obj state-obj] (resolve-expr state-val (:object expr))]
    [(assoc expr :value resolved-value :object resolved-obj) state-obj]))

(defmethod resolve-expr :this
  [state expr]
  (if (= (:class-type state) :none)
    (throw (ex-info "Can't use 'this' outside of a class." {:token (:keyword expr)}))
    [(resolve-local state expr (:keyword expr)) state]))

(defmethod resolve-expr :unary
  [state expr]
  (let [[right state-right] (resolve-expr state (:right expr))] [(assoc expr :right right) state-right]))

(defmethod resolve-expr :variable
  [state expr]
  (let [lexeme (:lexeme (:name expr))
        current-scope (first (:scopes state))]
    (if (and current-scope (= (get current-scope lexeme) false))
      (throw (ex-info "Can't read local variable in its own initializer." {:token (:name expr)}))
      [(resolve-local state expr (:name expr)) state])))

(defmethod resolve-expr :default [state expr] [expr state])

(defmethod resolve-stmt :block
  [state stmt]
  (with-scope [inner-state state]
              (let [[resolved-stmts state-after-stmts] (resolve-statements inner-state (:statements stmt))]
                [(assoc stmt :statements resolved-stmts) state-after-stmts])))

(defmethod resolve-stmt :class
  [state stmt]
  (let [class-name (:lexeme (:name stmt))
        state-declared (declare-var state class-name)
        state-defined (define-var state-declared class-name)
        saved-class-type (:class-type state-defined)
        ;; create a 'fake' scope for 'this' so methods can resolve it
        state-with-this (-> state-defined
                            (assoc :class-type :class)
                            (update :scopes #(cons {"this" true} %)))
        [resolved-methods _]
        (loop [remaining-methods (:methods stmt)
               current-state state-with-this
               resolved-methods-acc []]
          (if (empty? remaining-methods)
            [resolved-methods-acc current-state]
            (let [method (first remaining-methods)
                  method-type (if (= "init" (:lexeme (:name method))) :initialiser :method)
                  ;; resolve the method body without declaring its name in the scope
                  [resolved-method _] (resolve-function-body current-state method method-type)]
              (recur (rest remaining-methods) current-state (conj resolved-methods-acc resolved-method)))))]
    [(assoc stmt :methods resolved-methods)
     (assoc state-defined :class-type saved-class-type)]))

(defmethod resolve-stmt :expr
  [state stmt]
  (let [[expr state-expr] (resolve-expr state (:expression stmt))] [(assoc stmt :expression expr) state-expr]))

(defmethod resolve-stmt :function
  [state stmt]
  (let [func-name (:lexeme (:name stmt))
        state-declared (declare-var state func-name)
        state-defined (define-var state-declared func-name)]
    (resolve-function-body state-defined stmt :function)))

(defmethod resolve-stmt :if
  [state stmt]
  (let [[condition state-cond] (resolve-expr state (:condition stmt))
        [then-branch state-then] (resolve-stmt state-cond (:then-branch stmt))
        [else-branch state-else]
        (if (:else-branch stmt) (resolve-stmt state-then (:else-branch stmt)) [nil state-then])]
    [(assoc stmt :condition condition :then-branch then-branch :else-branch else-branch) state-else]))

(defmethod resolve-stmt :print
  [state stmt]
  (let [[expr state-expr] (resolve-expr state (:expression stmt))] [(assoc stmt :expression expr) state-expr]))

(defmethod resolve-stmt :return
  [state stmt]
  (if (:value stmt)
    (let [[value state-val] (resolve-expr state (:value stmt))] [(assoc stmt :value value) state-val])
    [stmt state]))

(defmethod resolve-stmt :var-stmt
  [state stmt]
  (let [lexeme (:lexeme (:name stmt))
        state-declared (declare-var state lexeme)
        [resolved-init state-after-init]
        (if (:initialiser stmt) (resolve-expr state-declared (:initialiser stmt)) [nil state-declared])
        state-defined (define-var state-after-init lexeme)]
    [(assoc stmt :initialiser resolved-init) state-defined]))

(defmethod resolve-stmt :while
  [state stmt]
  (let [[condition state-cond] (resolve-expr state (:condition stmt))
        [body state-body] (resolve-stmt state-cond (:body stmt))]
    [(assoc stmt :condition condition :body body) state-body]))

(defmethod resolve-stmt :default [state stmt] [stmt state])

(defn resolve [statements] (let [[resolved-stmts _] (resolve-statements initial-state statements)] resolved-stmts))
