(ns lox.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [lox.analyser :as analyser]
            [lox.evaluator :as evaluator]
            [lox.error :as error]
            [lox.memory :as memory]
            [lox.native :as native]
            [lox.parser :as parser]
            [lox.scanner :as scanner]
            [lox.resolver :as resolver]))

(defn- report-errors!
  [errors]
  (when (seq errors) (binding [*out* *err*] (doseq [err errors] (println (error/format-error err))))))

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

(defn- run
  [source env repl?]
  (if-let [ast (compile-ast source)]
    (try (loop [current-env env
                remaining-stmts ast]
           (if (empty? remaining-stmts)
             current-env
             (let [stmt (first remaining-stmts)
                   result (evaluator/execute stmt current-env)]
               (recur result (rest remaining-stmts)))))
         (catch Exception e
           (let [data (ex-data e)
                 err-type (:type data)]
             (binding [*out* *err*]
               (cond (= err-type :evaluator-error) (do (println
                                                        (str (.getMessage e) "\n[line " (:line (:token data)) "]"))
                                                       (if repl? env (System/exit 70)))
                     :else (do (println "System Error: " (.getMessage e)) (if repl? env (System/exit 1))))))))
    (if repl? env (System/exit 65))))

(defn- run-file
  [path]
  (let [file (io/file path)]
    (if (.exists file)
      (do (memory/empty-store!) (run (slurp file) (native/create-global-env) false))
      (println "File not found: " path))))

(defn- run-prompt
  []
  (memory/empty-store!)
  (loop [env (native/create-global-env)]
    (print "> ")
    (flush)
    (when-some [line (read-line)] (recur (run line env true)))))

(defn -main
  [& args]
  (try (let [arglen (count args)]
         (cond (> arglen 1) (do (println "Usage: cljlox [script]") (System/exit 64))
               (= arglen 1) (run-file (first args))
               :else (run-prompt)))
       (catch Exception e (println (format "Fatal error: %s" (ex-message e))))))
