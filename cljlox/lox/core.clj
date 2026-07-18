(ns lox.core
  (:require [clojure.java.io :as io]
            [lox.analyser :as analyser]
            [lox.environment :as environment]
            [lox.evaluator :as evaluator]
            [lox.memory :as memory]
            [lox.parser :as parser]
            [lox.scanner :as scanner]
            [lox.resolver :as resolver]))

(defn- run
  [source env]
  (let [tokens (scanner/scan source)
        scanner-errors (filter #(= (:type %) :error) tokens)]
    (if (seq scanner-errors)
      (do (doseq [err scanner-errors] (println (str "[line " (:line err) "] Error: " (:literal err)))) env)
      (try (let [[statements _] (parser/parse {:tokens tokens})
                 validated-statements (analyser/analyse statements)
                 resolved-statements (resolver/resolve validated-statements)]
             (loop [current-env env
                    remaining-stmts resolved-statements]
               (if (empty? remaining-stmts)
                 current-env
                 (let [stmt (first remaining-stmts)
                       result (evaluator/execute stmt current-env)]
                   (recur result (rest remaining-stmts))))))
           (catch Exception e
             (let [data (ex-data e)]
               (cond (:line data) (println (str "[line " (:line data) "] Error at parser: " (.getMessage e)))
                     (:token data) (println
                                    (str "Runtime Error: " (.getMessage e) "\n[line " (:line (:token data)) "]"))
                     :else (println "System Error: " (.getMessage e)))
               env))))))

(defn- run-file
  [path]
  (let [file (io/file path)]
    (if (.exists file)
      (do (memory/empty-store!) (run (slurp file) (environment/empty-env)))
      (println "File not found: " path))))

(defn- run-prompt
  []
  (memory/empty-store!)
  (loop [env (environment/empty-env)]
    (print "> ")
    (flush)
    (when-some [line (read-line)] (recur (run line env)))))

(defn -main
  [& args]
  (try (let [arglen (count args)]
         (cond (> arglen 1) (println "Usage: cljlox [script]")
               (= arglen 1) (run-file (first args))
               :else (run-prompt)))
       (catch Exception e (println (format "Fatal error: %s" (ex-message e))))))
