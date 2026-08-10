#_{:clj-kondo/ignore [:unused-namespace :unused-referred-var]}
(ns lox.evaluator
  (:require [lox.ast :as ast]
            [lox.environment :as environment]
            [lox.error :as error]
            [lox.memory :as memory]
            [lox.token :refer [TokenSchema]]))

(def EvaluatorStateSchema
  [:map
   [:env environment/EnvSchema]
   [:mem [:vector memory/ValueSchema]]
   [:stdout [:vector :string]]
   [:return [:maybe [:vector memory/ValueSchema]]]
   [:error [:maybe error/EvaluatorErrorSchema]]])

(def ^:private numeric-binary-ops #{:minus :slash :star :greater :greater-equal :less :less-equal})

(defn- validate-numeric!
  ([operator operand state]
   (if-not (number? operand) (assoc state :error (error/evaluator-error operator "Operand must be a number.")) state))
  ([operator left right state]
   (if-not (and (number? left) (number? right))
     (assoc state :error (error/evaluator-error operator "Operands must be numbers."))
     state)))

(defn- stringify
  [val]
  (cond (nil? val) "nil"
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
  {:malli/schema [:=> [:cat ast/ExprSchema EvaluatorStateSchema] [:tuple memory/ValueSchema EvaluatorStateSchema]]}
  (fn [expr _] (:type expr)))

(defmulti execute
  {:malli/schema [:=> [:cat ast/StmtSchema EvaluatorStateSchema] EvaluatorStateSchema]}
  (fn [stmt _] (:type stmt)))

(defn- make-function
  [stmt closure-env]
  (let [is-init? (and (= "init" (:lexeme (:name stmt))) (environment/has-local? closure-env "this"))
        invoke
        (fn [args caller-state]
          (let [base-env (environment/with-globals (environment/push-scope closure-env) (:env caller-state))
                [env-with-params mem-after-bind]
                (loop [env base-env
                       mem (:mem caller-state)
                       params (:params stmt)
                       arg-vals args]
                  (if (empty? params)
                    [env mem]
                    (let [{:keys [address mem new-mem]} (memory/alloc mem (first arg-vals))
                          m (or new-mem mem)
                          new-env (environment/bind-local-address env (:lexeme (first params)) address)]
                      (recur new-env m (rest params) (rest arg-vals)))))
                initial-state (assoc caller-state :env env-with-params :mem mem-after-bind)

                final-state (loop [current-state initial-state
                                   stmts (:body stmt)]
                              (if (empty? stmts)
                                current-state
                                (let [result-state (execute (first stmts) current-state)]
                                  (if (or (some? (:return result-state)) (some? (:error result-state)))
                                    result-state
                                    (recur result-state (rest stmts))))))]
            (if (:error final-state)
              [nil (assoc final-state :env (:env caller-state))]
              (let [ret-val
                    (if (:return final-state)
                      (if is-init?
                        (memory/read-store (:mem final-state) (environment/get-local-address closure-env "this"))
                        (first (:return final-state)))
                      (if is-init?
                        (memory/read-store (:mem final-state) (environment/get-local-address closure-env "this"))
                        nil))]
                [ret-val (assoc final-state :return nil :env (:env caller-state))]))))]
    {:type        :lox-function,
     :arity       (count (:params stmt)),
     :stmt        stmt,
     :closure-env closure-env,
     :call-fn     invoke}))

(defn- bind-method
  [method instance mem]
  (let [env-with-new-scope (environment/push-scope (:closure-env method))
        {:keys [address mem]} (memory/alloc mem instance)
        bound-env (environment/bind-local-address env-with-new-scope "this" address)]
    [(make-function (:stmt method) bound-env) mem]))

(defn- find-method
  [lox-class name-lexeme]
  (if-let [method (get-in lox-class [:methods name-lexeme])]
    method
    (when-let [superclass (:superclass lox-class)] (find-method superclass name-lexeme))))

(defmethod evaluate :assign
  [expr state]
  (let [[value state-after-val] (evaluate (:value expr) state)]
    (if (:error state-after-val)
      [nil state-after-val]
      (let [address (environment/resolve-address (:env state-after-val) (:name expr) (:depth expr))]
        (if (some? address)
          (let [new-mem (memory/write-store (:mem state-after-val) address value)]
            [value (assoc state-after-val :mem new-mem)])
          [nil
           (assoc state-after-val
                  :error
                  (error/evaluator-error (:name expr) (str "Undefined variable '" (:lexeme (:name expr)) "'.")))])))))

(defmethod evaluate :binary
  [expr state]
  (let [[left state-after-left] (evaluate (:left expr) state)]
    (if (:error state-after-left)
      [nil state-after-left]
      (let [[right state-after-right] (evaluate (:right expr) state-after-left)]
        (if (:error state-after-right)
          [nil state-after-right]
          (let [op (:op expr)
                op-type (:type op)]
            (if (contains? numeric-binary-ops op-type)
              (let [state-val (validate-numeric! op left right state-after-right)]
                (if (:error state-val)
                  [nil state-val]
                  (let [l (double left)
                        r (double right)]
                    [(cond (= op-type :minus) (- l r)
                           (= op-type :slash) (/ l r)
                           (= op-type :star) (* l r)
                           (= op-type :plus) (+ l r)
                           (= op-type :greater) (> l r)
                           (= op-type :greater-equal) (>= l r)
                           (= op-type :less) (< l r)
                           (= op-type :less-equal) (<= l r)
                           (= op-type :bang-equal) (not= l r)
                           (= op-type :equal-equal) (= l r))
                     state-val])))
              (if (= op-type :plus)
                (if (or (and (number? left) (number? right)) (and (string? left) (string? right)))
                  (if (and (number? left) (number? right))
                    [(+ (double left) (double right)) state-after-right]
                    [(str left right) state-after-right])
                  [nil
                   (assoc state-after-right
                          :error
                          (error/evaluator-error op "Operands must be two numbers or two strings."))])
                (cond (= op-type :bang-equal) [(not= left right) state-after-right]
                      (= op-type :equal-equal) [(= left right) state-after-right]
                      :else
                      [nil
                       (assoc state-after-right :error (error/evaluator-error op "Unknown binary operator"))])))))))))

(defmethod evaluate :call
  [expr state]
  (let [[callee state-after-callee] (evaluate (:callee expr) state)]
    (if (:error state-after-callee)
      [nil state-after-callee]
      (let [[args state-after-args]
            (loop [remaining-args (:arguments expr)
                   evaluated-args []
                   current-state state-after-callee]
              (if (empty? remaining-args)
                [evaluated-args current-state]
                (let [[v next-s] (evaluate (first remaining-args) current-state)]
                  (if (:error next-s) [nil next-s] (recur (rest remaining-args) (conj evaluated-args v) next-s)))))
            paren-token (:paren expr)]
        (if (:error state-after-args)
          [nil state-after-args]
          (cond (and (map? callee) (= (:type callee) :lox-function))
                (if-not (= (count args) (:arity callee))
                  [nil
                   (assoc state-after-args
                          :error
                          (error/evaluator-error
                           paren-token
                           (str "Expected " (:arity callee) " arguments but got " (count args) ".")))]
                  ((:call-fn callee) args state-after-args))
                (and (map? callee) (= (:type callee) :native-function))
                (if-not (= (count args) (:arity callee))
                  [nil
                   (assoc state-after-args
                          :error
                          (error/evaluator-error
                           paren-token
                           (str "Expected " (:arity callee) " arguments but got " (count args) ".")))]
                  (let [[v next-mem] ((:call-fn callee) args (:mem state-after-args))]
                    [v (assoc state-after-args :mem next-mem)]))
                (and (map? callee) (= (:type callee) :lox-class))
                (let [init-method (find-method callee "init")
                      arity (if init-method (:arity init-method) 0)]
                  (if-not (= (count args) arity)
                    [nil
                     (assoc state-after-args
                            :error
                            (error/evaluator-error paren-token
                                                   (str "Expected " arity " arguments but got " (count args) ".")))]
                    (let [{:keys [address mem]} (memory/alloc (:mem state-after-args) {})
                          instance {:type :lox-instance, :class callee, :fields-address address}
                          state-with-inst (assoc state-after-args :mem mem)]
                      (if init-method
                        (let [[bound-method mem-after-bind] (bind-method init-method instance (:mem state-with-inst))
                              [_ final-state]
                              ((:call-fn bound-method) args (assoc state-with-inst :mem mem-after-bind))]
                          [instance final-state])
                        [instance state-with-inst]))))
                :else [nil
                       (assoc state-after-args
                              :error
                              (error/evaluator-error paren-token "Can only call functions and classes."))]))))))

(defmethod evaluate :get
  [expr state]
  (let [[obj state-after-obj] (evaluate (:object expr) state)
        name-lexeme (:lexeme (:name expr))]
    (if (:error state-after-obj)
      [nil state-after-obj]
      (if (and (map? obj) (= (:type obj) :lox-instance))
        (let [fields (memory/read-store (:mem state-after-obj) (:fields-address obj))]
          (if (contains? fields name-lexeme)
            [(get fields name-lexeme) state-after-obj]
            (if-let [method (find-method (:class obj) name-lexeme)]
              (let [[bound-method new-mem] (bind-method method obj (:mem state-after-obj))]
                [bound-method (assoc state-after-obj :mem new-mem)])
              [nil
               (assoc state-after-obj
                      :error
                      (error/evaluator-error (:name expr) (str "Undefined property '" name-lexeme "'.")))])))
        [nil (assoc state-after-obj :error (error/evaluator-error (:name expr) "Only instances have properties."))]))))

(defmethod evaluate :grouping [expr state] (evaluate (:expression expr) state))

(defmethod evaluate :literal [expr state] [(:value expr) state])

(defmethod evaluate :logical
  [expr state]
  (let [[left-val state-after-left] (evaluate (:left expr) state)
        op-type (get-in expr [:op :type])]
    (if (:error state-after-left)
      [nil state-after-left]
      (if (= op-type :or)
        (if (truthy? left-val) [left-val state-after-left] (evaluate (:right expr) state-after-left))
        (if (not (truthy? left-val)) [left-val state-after-left] (evaluate (:right expr) state-after-left))))))

(defmethod evaluate :set
  [expr state]
  (let [[obj state-after-obj] (evaluate (:object expr) state)]
    (if (:error state-after-obj)
      [nil state-after-obj]
      (if (and (map? obj) (= (:type obj) :lox-instance))
        (let [[value state-after-val] (evaluate (:value expr) state-after-obj)]
          (if (:error state-after-val)
            [nil state-after-val]
            (let [name-lexeme (:lexeme (:name expr))
                  new-mem (memory/update-store (:mem state-after-val) (:fields-address obj) assoc name-lexeme value)]
              [value (assoc state-after-val :mem new-mem)])))
        [nil (assoc state-after-obj :error (error/evaluator-error (:name expr) "Only instances have fields."))]))))

(defmethod evaluate :super
  [expr state]
  (let [distance (:depth expr)
        super-token (:keyword expr)
        this-token {:type :this, :lexeme "this", :line (:line super-token)}
        superclass-addr (environment/resolve-address (:env state) super-token distance)
        instance-addr (environment/resolve-address (:env state) this-token (dec distance))]
    (if (and (some? superclass-addr) (some? instance-addr))
      (let [superclass (memory/read-store (:mem state) superclass-addr)
            instance (memory/read-store (:mem state) instance-addr)
            method-name (:lexeme (:method expr))
            method (find-method superclass method-name)]
        (if method
          (let [[bound-method new-mem] (bind-method method instance (:mem state))]
            [bound-method (assoc state :mem new-mem)])
          [nil
           (assoc state :error (error/evaluator-error (:method expr) (str "Undefined property '" method-name "'.")))]))
      [nil
       (assoc state
              :error
              (error/evaluator-error (if (some? superclass-addr) this-token super-token) "Undefined variable."))])))

(defmethod evaluate :this
  [expr state]
  (let [address (environment/resolve-address (:env state) (:keyword expr) (:depth expr))]
    (if (some? address)
      [(memory/read-store (:mem state) address) state]
      [nil
       (assoc state
              :error
              (error/evaluator-error (:keyword expr) (str "Undefined variable '" (:lexeme (:keyword expr)) "'.")))])))

(defmethod evaluate :unary
  [expr state]
  (let [[right state-after-right] (evaluate (:right expr) state)
        op (:op expr)
        op-type (get-in expr [:op :type])]
    (if (:error state-after-right)
      [nil state-after-right]
      (let [state-val (if (= op-type :minus) (validate-numeric! op right state-after-right) state-after-right)]
        (if (:error state-val)
          [nil state-val]
          [(cond (= op-type :minus) (- (double right))
                 (= op-type :bang) (not (truthy? right))
                 :else nil)
           state-val])))))

(defmethod evaluate :variable
  [expr state]
  (let [address (environment/resolve-address (:env state) (:name expr) (:depth expr))]
    (if (some? address)
      [(memory/read-store (:mem state) address) state]
      [nil
       (assoc state
              :error
              (error/evaluator-error (:name expr) (str "Undefined variable '" (:lexeme (:name expr)) "'.")))])))


(defmethod execute :block
  [stmt state]
  (let [inner-env (environment/push-scope (:env state))
        final-state (loop [current-state (assoc state :env inner-env)
                           remaining-stmts (:statements stmt)]
                      (if (empty? remaining-stmts)
                        current-state
                        (let [s (first remaining-stmts)
                              result-state (execute s current-state)]
                          (if (or (some? (:return result-state)) (some? (:error result-state)))
                            result-state
                            (recur result-state (rest remaining-stmts))))))]
    (assoc final-state :env (:env state))))

(defmethod execute :class
  [stmt state]
  (let [superclass-expr (:superclass stmt)
        [superclass state-after-super] (if superclass-expr (evaluate superclass-expr state) [nil state])]
    (if (:error state-after-super)
      state-after-super
      (if (and superclass-expr (not= (:type superclass) :lox-class))
        (assoc state-after-super :error (error/evaluator-error (:name superclass-expr) "Superclass must be a class."))
        (let [lexeme (:lexeme (:name stmt))
              {:keys [address mem]} (memory/alloc (:mem state-after-super) nil)
              new-env (environment/bind-local-address (:env state-after-super) lexeme address)

              [closure-env mem-after-super]
              (if superclass
                (let [{a2 :address, m2 :mem} (memory/alloc mem superclass)]
                  [(environment/bind-local-address (environment/push-scope new-env) "super" a2) m2])
                [new-env mem])

              methods (loop [remaining (:methods stmt)
                             acc {}]
                        (if (empty? remaining)
                          acc
                          (let [method (first remaining)
                                method-name (:lexeme (:name method))
                                func (make-function method closure-env)]
                            (recur (rest remaining) (assoc acc method-name func)))))
              lox-class {:type :lox-class, :name lexeme, :superclass superclass, :methods methods}
              final-mem (memory/write-store mem-after-super address lox-class)]
          (assoc state-after-super :env new-env :mem final-mem))))))

(defmethod execute :expr [stmt state] (let [[_ new-state] (evaluate (:expression stmt) state)] new-state))

(defmethod execute :function
  [stmt state]
  (let [lexeme (:lexeme (:name stmt))
        {:keys [address mem]} (memory/alloc (:mem state) nil)
        new-env (environment/bind-local-address (:env state) lexeme address)
        func (make-function stmt new-env)
        final-mem (memory/write-store mem address func)]
    (assoc state :env new-env :mem final-mem)))

(defmethod execute :if
  [stmt state]
  (let [[cond-val state-after-cond] (evaluate (:condition stmt) state)]
    (if (:error state-after-cond)
      state-after-cond
      (if (truthy? cond-val)
        (execute (:then-branch stmt) state-after-cond)
        (if (:else-branch stmt) (execute (:else-branch stmt) state-after-cond) state-after-cond)))))

(defmethod execute :print
  [stmt state]
  (let [[value state-after-eval] (evaluate (:expression stmt) state)]
    (if (:error state-after-eval)
      state-after-eval
      (let [out-str (if (nil? value) "nil" (stringify value))]
        (assoc state-after-eval :stdout (conj (:stdout state-after-eval) out-str))))))

(defmethod execute :return
  [stmt state]
  (let [[value state-after-eval] (if (:value stmt) (evaluate (:value stmt) state) [nil state])]
    (if (:error state-after-eval) state-after-eval (assoc state-after-eval :return [value]))))

(defmethod execute :var-stmt
  [stmt state]
  (let [[value state-after-eval] (if (:initialiser stmt) (evaluate (:initialiser stmt) state) [nil state])]
    (if (:error state-after-eval)
      state-after-eval
      (let [{:keys [address mem]} (memory/alloc (:mem state-after-eval) value)
            new-env (environment/bind-local-address (:env state-after-eval) (:lexeme (:name stmt)) address)]
        (assoc state-after-eval :env new-env :mem mem)))))

(defmethod execute :while
  [stmt state]
  (loop [current-state state]
    (let [[cond-val state-after-cond] (evaluate (:condition stmt) current-state)]
      (if (:error state-after-cond)
        state-after-cond
        (if (truthy? cond-val)
          (let [result-state (execute (:body stmt) state-after-cond)]
            (if (or (some? (:return result-state)) (some? (:error result-state))) result-state (recur result-state)))
          state-after-cond)))))
