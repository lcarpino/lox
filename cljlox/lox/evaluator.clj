(ns lox.evaluator
  (:require [lox.ast :as ast]
            [lox.environment :as environment]
            [lox.memory :as memory]))

(def ReturnSchema [:map [:type [:= :return-value]] [:value memory/ValueSchema]])

(defn- truthy? [val] (not (or (nil? val) (false? val))))

(defmulti evaluate
  {:malli/schema [:=> [:cat ast/ExprSchema environment/EnvSchema] memory/ValueSchema]}
  (fn [expr env] (:type expr)))

(defmulti execute
  {:malli/schema [:=> [:cat ast/StmtSchema environment/EnvSchema] [:or environment/EnvSchema ReturnSchema]]}
  (fn [stmt env] (:type stmt)))

(defn- make-function
  [stmt closure-env]
  (let [is-init? (and (= "init" (:lexeme (:name stmt))) (contains? (first closure-env) "this"))
        invoke (fn [args caller-env]
                 (let [stitched-env (concat (drop-last closure-env) [(last caller-env)])
                       base-env (cons {} stitched-env)
                       env-with-params (loop [env base-env
                                              params (:params stmt)
                                              arg-vals args]
                                         (if (empty? params)
                                           env
                                           (recur (environment/define env (first params) (first arg-vals))
                                                  (rest params)
                                                  (rest arg-vals))))]
                   (loop [current-env env-with-params
                          stmts (:body stmt)]
                     (if (empty? stmts)
                       (if is-init? (memory/read-store (get (first closure-env) "this")) nil)
                       (let [result (execute (first stmts) current-env)]
                         (if (and (map? result) (= (:type result) :return-value))
                           (if is-init? (memory/read-store (get (first closure-env) "this")) (:value result))
                           (recur result (rest stmts))))))))]
    {:type        :lox-function,
     :arity       (count (:params stmt)),
     :stmt        stmt,
     :closure-env closure-env,
     :call-fn     invoke}))

(defn- bind-method
  [method instance]
  (let [env-with-new-scope (cons {} (:closure-env method))
        bound-env (environment/define env-with-new-scope {:lexeme "this"} instance)]
    (make-function (:stmt method) bound-env)))

(defmethod evaluate :assign
  [expr env]
  (let [value (evaluate (:value expr) env)
        address (environment/resolve-address env (:name expr) (:depth expr))]
    (memory/write-store! address value)
    value))

(defmethod evaluate :binary
  [expr env]
  (let [left (evaluate (:left expr) env)
        right (evaluate (:right expr) env)
        op-type (get-in expr [:op :type])]
    (cond (= op-type :minus) (- left right)
          (= op-type :slash) (/ left right)
          (= op-type :star) (* left right)
          (= op-type :greater) (> left right)
          (= op-type :greater-equal) (>= left right)
          (= op-type :less) (< left right)
          (= op-type :less-equal) (<= left right)
          (= op-type :bang-equal) (not= left right)
          (= op-type :equal-equal) (= left right)
          (= op-type :plus) (if (and (string? left) (string? right)) (str left right) (+ left right))
          :else (throw (ex-info "Unknown binary operator" {:node expr})))))

(defmethod evaluate :call
  [expr env]
  (let [callee (evaluate (:callee expr) env)
        args (loop [remaining-args (:arguments expr)
                    evaluated-args []]
               (if (empty? remaining-args)
                 evaluated-args
                 (recur (rest remaining-args) (conj evaluated-args (evaluate (first remaining-args) env)))))
        paren-token (:paren expr)]
    (cond (and (map? callee) (= (:type callee) :lox-function))
          (if-not (= (count args) (:arity callee))
            (throw (ex-info (str "Expected " (:arity callee) " arguments but got " (count args) ".")
                            {:token paren-token}))
            ((:call-fn callee) args env))
          (and (map? callee) (= (:type callee) :lox-class))
          (let [init-method (get-in callee [:methods "init"])
                arity (if init-method (:arity init-method) 0)]
            (if-not (= (count args) arity)
              (throw (ex-info (str "Expected " arity " arguments but got " (count args) ".") {:token paren-token}))
              (let [fields-address (memory/alloc! {})
                    instance {:type :lox-instance, :class callee, :fields-address fields-address}]
                (when init-method ((:call-fn (bind-method init-method instance)) args env))
                instance)))
          :else (throw (ex-info "Can only call functions and classes." {:token paren-token})))))

(defmethod evaluate :get
  [expr env]
  (let [obj (evaluate (:object expr) env)
        name-lexeme (:lexeme (:name expr))]
    (if (and (map? obj) (= (:type obj) :lox-instance))
      (let [fields (memory/read-store (:fields-address obj))]
        (if (contains? fields name-lexeme)
          (get fields name-lexeme)
          (if-let [method (get-in obj [:class :methods name-lexeme])]
            (bind-method method obj)
            (throw (ex-info (str "Undefined property '" name-lexeme "'.") {:token (:name expr)})))))
      (throw (ex-info "Only instances have properties." {:token (:name expr)})))))

(defmethod evaluate :grouping [expr env] (evaluate (:expression expr) env))

(defmethod evaluate :literal [expr env] (:value expr))

(defmethod evaluate :logical
  [expr env]
  (let [left-val (evaluate (:left expr) env)
        op-type (get-in expr [:op :type])]
    (if (= op-type :or)
      (if (truthy? left-val) left-val (evaluate (:right expr) env))
      (if (not (truthy? left-val)) left-val (evaluate (:right expr) env)))))

(defmethod evaluate :set
  [expr env]
  (let [obj (evaluate (:object expr) env)]
    (if (and (map? obj) (= (:type obj) :lox-instance))
      (let [value (evaluate (:value expr) env)
            name-lexeme (:lexeme (:name expr))
            current-fields (memory/read-store (:fields-address obj))
            updated-fields (assoc current-fields name-lexeme value)]
        (memory/write-store! (:fields-address obj) updated-fields)
        value)
      (throw (ex-info "Only instances have fields." {:token (:name expr)})))))

(defmethod evaluate :this
  [expr env]
  (let [address (environment/resolve-address env (:keyword expr) (:depth expr))] (memory/read-store address)))

(defmethod evaluate :unary
  [expr env]
  (let [right (evaluate (:right expr) env)
        op-type (get-in expr [:op :type])]
    (cond (= op-type :minus) (- right)
          (= op-type :bang) (not (truthy? right))
          :else (throw (ex-info "Unknown unary operator" {:node expr})))))

(defmethod evaluate :variable
  [expr env]
  (let [address (environment/resolve-address env (:name expr) (:depth expr))] (memory/read-store address)))

(defmethod execute :block
  [stmt env]
  (let [inner-env (cons {} env)]
    (loop [current-env inner-env
           remaining-stmts (:statements stmt)]
      (if (empty? remaining-stmts)
        (rest current-env)
        (let [s (first remaining-stmts)
              result (execute s current-env)]
          (if (and (map? result) (= (:type result) :return-value)) result (recur result (rest remaining-stmts))))))))

(defmethod execute :class
  [stmt env]
  (let [lexeme (:lexeme (:name stmt))
        address (memory/alloc! nil)
        new-env (cons (assoc (first env) lexeme address) (rest env))
        methods (loop [remaining (:methods stmt)
                       acc {}]
                  (if (empty? remaining)
                    acc
                    (let [method (first remaining)
                          method-name (:lexeme (:name method))
                          func (make-function method new-env)]
                      (recur (rest remaining) (assoc acc method-name func)))))
        lox-class {:type :lox-class, :name lexeme, :methods methods}]
    (memory/write-store! address lox-class)
    new-env))

(defmethod execute :expr [stmt env] (evaluate (:expression stmt) env) env)

(defmethod execute :function
  [stmt env]
  (let [lexeme (:lexeme (:name stmt))
        address (memory/alloc! nil)
        new-env (cons (assoc (first env) lexeme address) (rest env))
        func (make-function stmt new-env)]
    (memory/write-store! address func)
    new-env))

(defmethod execute :if
  [stmt env]
  (if (truthy? (evaluate (:condition stmt) env))
    (execute (:then-branch stmt) env)
    (if (:else-branch stmt) (execute (:else-branch stmt) env) env)))

(defmethod execute :print
  [stmt env]
  (let [value (evaluate (:expression stmt) env)]
    (println (if (nil? value) "nil" (str value)))
    env))

(defmethod execute :return
  [stmt env]
  (let [value (if (:value stmt) (evaluate (:value stmt) env) nil)] {:type :return-value, :value value}))

(defmethod execute :var-stmt
  [stmt env]
  (let [value (if (:initialiser stmt) (evaluate (:initialiser stmt) env) nil)]
    (environment/define env (:name stmt) value)))

(defmethod execute :while
  [stmt env]
  (loop [current-env env]
    (if (truthy? (evaluate (:condition stmt) current-env))
      (let [result (execute (:body stmt) current-env)]
        (if (and (map? result) (= (:type result) :return-value)) result (recur result)))
      current-env)))
