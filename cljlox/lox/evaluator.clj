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

(defmethod evaluate :literal [expr env] (:value expr))

(defmethod evaluate :grouping [expr env] (evaluate (:expression expr) env))

(defmethod evaluate :variable
  [expr env]
  (let [address (environment/resolve-address env (:name expr) (:depth expr))] (memory/read-store address)))

(defmethod evaluate :assign
  [expr env]
  (let [value (evaluate (:value expr) env)
        address (environment/resolve-address env (:name expr) (:depth expr))]
    (memory/write-store! address value)
    value))

(defmethod evaluate :unary
  [expr env]
  (let [right (evaluate (:right expr) env)
        op-type (get-in expr [:op :type])]
    (cond (= op-type :minus) (- right)
          (= op-type :bang) (not (truthy? right))
          :else (throw (ex-info "Unknown unary operator" {:node expr})))))

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

(defmethod evaluate :logical
  [expr env]
  (let [left-val (evaluate (:left expr) env)
        op-type (get-in expr [:op :type])]
    (if (= op-type :or)
      (if (truthy? left-val) left-val (evaluate (:right expr) env))
      (if (not (truthy? left-val)) left-val (evaluate (:right expr) env)))))

(defmethod evaluate :call
  [expr env]
  (let [callee (evaluate (:callee expr) env)
        args (map #(evaluate % env) (:arguments expr))]
    (if-not (and (map? callee) (= (:type callee) :lox-function))
      (throw (ex-info "Can only call functions and classes." {:token (:paren expr)}))
      (if-not (= (count args) (:arity callee))
        (throw (ex-info (str "Expected " (:arity callee) " arguments but got " (count args) ".")
                        {:token (:paren expr)}))
        ((:call-fn callee) args env)))))

(defmethod execute :expr [stmt env] (evaluate (:expression stmt) env) env)

(defmethod execute :print
  [stmt env]
  (let [value (evaluate (:expression stmt) env)]
    (println (if (nil? value) "nil" (str value)))
    env))

(defmethod execute :var-stmt
  [stmt env]
  (let [value (if (:initialiser stmt) (evaluate (:initialiser stmt) env) nil)]
    (environment/define env (:name stmt) value)))

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

(defmethod execute :if
  [stmt env]
  (if (truthy? (evaluate (:condition stmt) env))
    (execute (:then-branch stmt) env)
    (if (:else-branch stmt) (execute (:else-branch stmt) env) env)))

(defmethod execute :while
  [stmt env]
  (loop [current-env env]
    (if (truthy? (evaluate (:condition stmt) current-env))
      (let [result (execute (:body stmt) current-env)]
        (if (and (map? result) (= (:type result) :return-value)) result (recur result)))
      current-env)))

(defn- make-function
  [stmt closure-env]
  {:type    :lox-function,
   :arity   (count (:params stmt)),
   :call-fn (fn [args caller-env]
              (let [stitched-env (concat (drop-last closure-env) [(last caller-env)])
                    env-with-params (reduce (fn [env [param-token arg-val]]
                                              (environment/define env param-token arg-val))
                                            (cons {} stitched-env)
                                            (map vector (:params stmt) args))]
                (loop [current-env env-with-params
                       stmts (:body stmt)]
                  (if (empty? stmts)
                    nil
                    (let [result (execute (first stmts) current-env)]
                      (if (and (map? result) (= (:type result) :return-value))
                        (:value result)
                        (recur result (rest stmts))))))))})


(defmethod execute :function
  [stmt env]
  (let [lexeme (:lexeme (:name stmt))
        address (memory/alloc! nil)
        new-env (cons (assoc (first env) lexeme address) (rest env))
        func (make-function stmt new-env)]
    (memory/write-store! address func)
    new-env))

(defmethod execute :return
  [stmt env]
  (let [value (if (:value stmt) (evaluate (:value stmt) env) nil)] {:type :return-value, :value value}))
