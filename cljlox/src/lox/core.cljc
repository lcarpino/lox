(ns lox.core
  (:require [lox.analyser :as analyser]
            [lox.error :as error]
            [lox.evaluator :as evaluator]
            [lox.parser :as parser]
            [lox.scanner :as scanner]
            [lox.resolver :as resolver]
            [lox.ast :as ast]
            [lox.memory :as memory]
            [lox.native :as native]))

(defn create-initial-state
  {:malli/schema [:=> [:cat] evaluator/EvaluatorStateSchema]}
  []
  (loop [funcs (seq native/native-functions)
         env {}
         mem []]
    (if (empty? funcs)
      {:env    {:locals [], :globals env},
       :mem    mem,
       :stdout [],
       :return nil,
       :error  nil}
      (let [[name-str func-map] (first funcs)
            {:keys [address mem]} (memory/alloc mem func-map)]
        (recur (rest funcs) (assoc env name-str address) mem)))))

(defn- report-errors!
  {:malli/schema [:=> [:cat [:sequential error/ErrorSchema]] :nil]}
  [errors]
  (when (seq errors)
    (binding [#?@(:clj [*out* *err*]
                  :cljs [*print-fn* *print-err-fn*])]
      (doseq [err errors] (println (error/format-error err))))))

(defn- compile-ast
  {:malli/schema [:=> [:cat :string] [:maybe [:sequential ast/StmtSchema]]]}
  [source]
  (let [[tokens scanner-state] (scanner/scan source)
        [statements parser-state] (parser/parse {:tokens tokens, :errors []})
        scanner-errors (:errors scanner-state)
        parser-errors (:errors parser-state)]
    (report-errors! scanner-errors)
    (report-errors! parser-errors)
    (if (or (seq scanner-errors) (seq parser-errors))
      nil
      (let [[validated analyser-state] (analyser/analyse statements)
            analyser-errors (:errors analyser-state)]
        (report-errors! analyser-errors)
        (if (seq analyser-errors)
          nil
          (let [[resolved resolver-state] (resolver/resolve validated)
                resolver-errors (:errors resolver-state)]
            (report-errors! resolver-errors)
            (if (seq resolver-errors) nil resolved)))))))

(defn execute
  {:malli/schema [:=> [:cat :string evaluator/EvaluatorStateSchema] evaluator/EvaluatorStateSchema]}
  [source state]
  (if-let [ast (compile-ast source)]
    (let [final-state (reduce (fn [current-state stmt]
                                (if (:error current-state) current-state (evaluator/execute stmt current-state)))
                              (assoc state :stdout [])
                              ast)]
      (doseq [line (:stdout final-state)] (println line))
      (if-let [err (:error final-state)]
        (do (binding [#?@(:clj [*out* *err*]
                          :cljs [*print-fn* *print-err-fn*])]
              (println (error/format-error err)))
            (assoc final-state :exit-code 70))
        (assoc final-state :exit-code nil)))
    (assoc state :exit-code 65)))
