(ns lox.evaluator
  (:require [lox.environment :as environment]
            [lox.memory :as memory]))

(defn- truthy? [val] (not (or (nil? val) (false? val))))

(defmulti evaluate (fn [expr env] (:type expr)))

(defmethod evaluate :literal [expr env] (:value expr))

(defmethod evaluate :grouping [expr env] (evaluate (:expression expr) env))

(defmethod evaluate :variable [expr env]
  (let [address (environment/resolve-address env (:name expr))]
    (memory/read-store address)))

(defmethod evaluate :assign [expr env]
  (let [value (evaluate (:value expr) env)
        address (environment/resolve-address env (:name expr))]
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

(defmulti execute (fn [stmt env] (:type stmt)))

(defmethod execute :expr [stmt env] (evaluate (:expression stmt) env) env)

(defmethod execute :print
  [stmt env]
  (let [value (evaluate (:expression stmt) env)]
    (println (if (nil? value) "nil" (str value)))
    env))

(defmethod execute :var-stmt [stmt env]
  (let [value (if (:initialiser stmt)
                (evaluate (:initialiser stmt) env)
                nil)]
    (environment/define env (:name stmt) value)))

(defn interpret
  [statements]
  (memory/empty-store!)
  (try
    (loop [env (environment/empty-env)
           remaining-stmts statements]
      (if (empty? remaining-stmts)
        nil
        (let [stmt (first remaining-stmts)
              new-env (execute stmt env)]
          (recur new-env (rest remaining-stmts)))))
  (catch Exception e
    (let [token (:token (ex-data e))]
      (if token
        (println (str "Runtime Error: " (.getMessage e) "\n[line " (:line token) "]"))
        (println "JVM Error: " (.getMessage e)))))))
