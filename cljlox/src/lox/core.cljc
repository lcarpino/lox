(ns lox.core
  (:require [lox.analyser :as analyser]
            [lox.error :as error]
            [lox.evaluator :as evaluator]
            [lox.parser :as parser]
            [lox.scanner :as scanner]
            [lox.resolver :as resolver]))

(defn- report-errors!
  [errors]
  (when (seq errors)
    #?(:clj (binding [*out* *err*] (doseq [err errors] (println (error/format-error err))))
       :cljs (doseq [err errors] (js/console.error (error/format-error err))))))

(defn- compile-ast
  [source]
  (let [[tokens scanner-state] (scanner/scan source)
        [statements parser-state] (parser/parse {:tokens tokens})
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
  [source env]
  (if-let [ast (compile-ast source)]
    (try (loop [current-env env
                remaining-stmts ast]
           (if (empty? remaining-stmts)
             {:env current-env}
             (let [stmt (first remaining-stmts)
                   result (evaluator/execute stmt current-env)]
               (recur result (rest remaining-stmts)))))
         (catch #?(:clj Exception
                   :cljs js/Error)
           e
           (let [data (ex-data e)
                 err-type (:type data)]
             (cond (= err-type :evaluator-error)
                   (do #?(:clj (binding [*out* *err*]
                                 (println (str (ex-message e) "\n[line " (:line (:token data)) "]")))
                          :cljs (js/console.error (str (ex-message e) "\n[line " (:line (:token data)) "]")))
                       {:env env, :exit-code 70})
                   :else (do #?(:clj (binding [*out* *err*] (println "System Error: " (ex-message e)))
                                :cljs (js/console.error "System Error: " (ex-message e)))
                             {:env env, :exit-code 1})))))
    {:env env, :exit-code 65}))
