(ns lox.evaluator
  (:require #_{:clj-kondo/ignore [:unused-namespace]}
            [lox.ast :as ast]
            [lox.environment :as environment]
            [lox.error :as error]
            [lox.memory :as memory]))

(def ReturnSchema [:map [:type [:= :return-value]] [:value memory/ValueSchema]])

(def ^:private numeric-binary-ops #{:minus :slash :star :greater :greater-equal :less :less-equal})

(defn- validate-numeric!
  ([operator operand] (when-not (number? operand) (error/evaluator-error operator "Operand must be a number.")))
  ([operator left right]
   (when-not (and (number? left) (number? right)) (error/evaluator-error operator "Operands must be numbers."))))

(defn- stringify
  [val]
  (cond (nil? val) "nil"
        ;; NOTE: this is actually necessary because in Clojure 0.0 == +0.0 == -0.0
        (number? val) #?(:clj (if (== val 0.0)
                                (if (= (Double/doubleToRawLongBits val) (Double/doubleToRawLongBits -0.0))
                                  "-0"
                                  (if (== val (Math/floor val)) (format "%d" (long val)) (str val)))
                                (if (== val (Math/floor val)) (format "%d" (long val)) (str val)))
                         :cljs (if (js/Object.is val -0.0) "-0" (str val)))
        (and (map? val) (= (:type val) :lox-class)) (:name val)
        (and (map? val) (= (:type val) :lox-instance)) (str (get-in val [:class :name]) " instance")
        (and (map? val) (= (:type val) :lox-function)) (str "<fn " (:lexeme (:name (:stmt val))) ">")
        (and (map? val) (= (:type val) :native-function)) "<native fn>"
        :else (str val)))

(defn- truthy? [val] (not (or (nil? val) (false? val))))

(defmulti evaluate
  {:malli/schema [:=> [:cat ast/ExprSchema environment/EnvSchema] memory/ValueSchema]}
  (fn [expr _] (:type expr)))

(defmulti execute
  {:malli/schema [:=> [:cat ast/StmtSchema environment/EnvSchema] [:or environment/EnvSchema ReturnSchema]]}
  (fn [stmt _] (:type stmt)))

(defn- make-function
  [stmt closure-env]
  (let [is-init? (and (= "init" (:lexeme (:name stmt))) (contains? (first (:locals closure-env)) "this"))
        invoke (fn [args caller-env]
                 (let [base-env {:locals  (cons {} (:locals closure-env)),
                                 :globals (:globals caller-env)}
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
                       (if is-init? (memory/read-store (get (first (:locals closure-env)) "this")) nil)
                       (let [result (execute (first stmts) current-env)]
                         (if (and (map? result) (= (:type result) :return-value))
                           (if is-init? (memory/read-store (get (first (:locals closure-env)) "this")) (:value result))
                           (recur result (rest stmts))))))))]
    {:type        :lox-function,
     :arity       (count (:params stmt)),
     :stmt        stmt,
     :closure-env closure-env,
     :call-fn     invoke}))

(defn- bind-method
  [method instance]
  (let [env-with-new-scope (assoc (:closure-env method) :locals (cons {} (:locals (:closure-env method))))
        method-name-token (:name (:stmt method))
        this-token {:type :this, :lexeme "this", :line (:line method-name-token)}
        bound-env (environment/define env-with-new-scope this-token instance)]
    (make-function (:stmt method) bound-env)))

(defn- find-method
  [lox-class name-lexeme]
  (if-let [method (get-in lox-class [:methods name-lexeme])]
    method
    (when-let [superclass (:superclass lox-class)] (find-method superclass name-lexeme))))

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
        op (:op expr)
        op-type (get-in expr [:op :type])]
    (cond (contains? numeric-binary-ops op-type) (validate-numeric! op left right)
          (= op-type :plus) (when-not (or (and (number? left) (number? right)) (and (string? left) (string? right)))
                              (error/evaluator-error op "Operands must be two numbers or two strings.")))
    (if (and (number? left) (number? right))
      (let [l (double left)
            r (double right)]
        (cond (= op-type :minus) (- l r)
              (= op-type :slash) (/ l r)
              (= op-type :star) (* l r)
              (= op-type :plus) (+ l r)
              (= op-type :greater) (> l r)
              (= op-type :greater-equal) (>= l r)
              (= op-type :less) (< l r)
              (= op-type :less-equal) (<= l r)
              (= op-type :bang-equal) (not= l r)
              (= op-type :equal-equal) (= l r)
              :else (error/evaluator-error op "Unknown binary operator")))
      (cond (= op-type :plus) (str left right)
            (= op-type :bang-equal) (not= left right)
            (= op-type :equal-equal) (= left right)
            :else (error/evaluator-error op "Unknown binary operator")))))

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
            (error/evaluator-error paren-token (str "Expected " (:arity callee) " arguments but got " (count args) "."))
            ((:call-fn callee) args env))
          (and (map? callee) (= (:type callee) :native-function))
          (if-not (= (count args) (:arity callee))
            (error/evaluator-error paren-token (str "Expected " (:arity callee) " arguments but got " (count args) "."))
            ((:call-fn callee) args))
          (and (map? callee) (= (:type callee) :lox-class))
          (let [init-method (find-method callee "init")
                arity (if init-method (:arity init-method) 0)]
            (if-not (= (count args) arity)
              (error/evaluator-error paren-token (str "Expected " arity " arguments but got " (count args) "."))
              (let [fields-address (memory/alloc! {})
                    instance {:type :lox-instance, :class callee, :fields-address fields-address}]
                (when init-method ((:call-fn (bind-method init-method instance)) args env))
                instance)))
          :else (error/evaluator-error paren-token "Can only call functions and classes."))))

(defmethod evaluate :get
  [expr env]
  (let [obj (evaluate (:object expr) env)
        name-lexeme (:lexeme (:name expr))]
    (if (and (map? obj) (= (:type obj) :lox-instance))
      (let [fields (memory/read-store (:fields-address obj))]
        (if (contains? fields name-lexeme)
          (get fields name-lexeme)
          (if-let [method (find-method (:class obj) name-lexeme)]
            (bind-method method obj)
            (error/evaluator-error (:name expr) (str "Undefined property '" name-lexeme "'.")))))
      (error/evaluator-error (:name expr) "Only instances have properties."))))

(defmethod evaluate :grouping [expr env] (evaluate (:expression expr) env))

(defmethod evaluate :literal [expr _] (:value expr))

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
      (error/evaluator-error (:name expr) "Only instances have fields."))))

(defmethod evaluate :super
  [expr env]
  (let [distance (:depth expr)
        super-token (:keyword expr)
        this-token {:type :this, :lexeme "this", :line (:line super-token)}
        superclass (memory/read-store (environment/resolve-address env super-token distance))
        instance (memory/read-store (environment/resolve-address env this-token (dec distance)))
        method-name (:lexeme (:method expr))
        method (find-method superclass method-name)]
    (if method
      (bind-method method instance)
      (error/evaluator-error (:method expr) (str "Undefined property '" method-name "'.")))))

(defmethod evaluate :this
  [expr env]
  (let [address (environment/resolve-address env (:keyword expr) (:depth expr))] (memory/read-store address)))

(defmethod evaluate :unary
  [expr env]
  (let [right (evaluate (:right expr) env)
        op (:op expr)
        op-type (get-in expr [:op :type])]
    (when (= op-type :minus) (validate-numeric! op right))
    (cond (= op-type :minus) (- (double right))
          (= op-type :bang) (not (truthy? right))
          :else (error/evaluator-error op "Unknown unary operator"))))

(defmethod evaluate :variable
  [expr env]
  (let [address (environment/resolve-address env (:name expr) (:depth expr))] (memory/read-store address)))

(defmethod execute :block
  [stmt env]
  (let [inner-env (assoc env :locals (cons {} (:locals env)))]
    (loop [current-env inner-env
           remaining-stmts (:statements stmt)]
      (if (empty? remaining-stmts)
        (assoc current-env :locals (rest (:locals current-env)))
        (let [s (first remaining-stmts)
              result (execute s current-env)]
          (if (and (map? result) (= (:type result) :return-value)) result (recur result (rest remaining-stmts))))))))

(defmethod execute :class
  [stmt env]
  (let [superclass-expr (:superclass stmt)
        superclass (when superclass-expr (evaluate superclass-expr env))]
    (when (and superclass-expr (not= (:type superclass) :lox-class))
      (error/evaluator-error (:name superclass-expr) "Superclass must be a class."))
    (let [lexeme (:lexeme (:name stmt))
          address (memory/alloc! nil)
          locals (:locals env)
          new-env (if (empty? locals)
                    (assoc-in env [:globals lexeme] address)
                    (assoc env :locals (cons (assoc (first locals) lexeme address) (rest locals))))
          closure-env
          (if superclass (assoc new-env :locals (cons {"super" (memory/alloc! superclass)} (:locals new-env))) new-env)
          methods (loop [remaining (:methods stmt)
                         acc {}]
                    (if (empty? remaining)
                      acc
                      (let [method (first remaining)
                            method-name (:lexeme (:name method))
                            func (make-function method closure-env)]
                        (recur (rest remaining) (assoc acc method-name func)))))
          lox-class {:type :lox-class, :name lexeme, :superclass superclass, :methods methods}]
      (memory/write-store! address lox-class)
      new-env)))

(defmethod execute :expr [stmt env] (evaluate (:expression stmt) env) env)

(defmethod execute :function
  [stmt env]
  (let [lexeme (:lexeme (:name stmt))
        address (memory/alloc! nil)
        locals (:locals env)
        new-env (if (empty? locals)
                  (assoc-in env [:globals lexeme] address)
                  (assoc env :locals (cons (assoc (first locals) lexeme address) (rest locals))))
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
    (println (if (nil? value) "nil" (stringify value)))
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
