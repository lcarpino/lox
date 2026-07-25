(ns lox.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [lox.analyser :as analyser]
            [lox.evaluator :as evaluator]
            [lox.memory :as memory]
            [lox.native :as native]
            [lox.parser :as parser]
            [lox.scanner :as scanner]
            [lox.resolver :as resolver]))

(defn- run
  [source env repl?]
  (let [tokens (scanner/scan source)
        scanner-errors (filter #(= (:type %) :error) tokens)]
    (if (seq scanner-errors)
      (do (binding [*out* *err*]
            (doseq [err scanner-errors] (println (str "[line " (:line err) "] Error: " (:literal err)))))
          (if repl? env (System/exit 65)))
      (try
        (let [[statements _] (parser/parse {:tokens tokens})
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
          (let [data (ex-data e)
                err-type (:type data)]
            (binding [*out* *err*]
              (cond (= err-type :analyser-error) (do (println (str "[line " (:line (:token data))
                                                                   "] Error at '" (:lexeme (:token data))
                                                                   "': " (.getMessage e)))
                                                     (if repl? env (System/exit 65)))
                    (= err-type :resolver-error) (do (println (str "[line " (:line (:token data))
                                                                   "] Error at '" (:lexeme (:token data))
                                                                   "': " (.getMessage e)))
                                                     (if repl? env (System/exit 65)))
                    (= err-type :evaluator-error) (do (println
                                                       (str (.getMessage e) "\n[line " (:line (:token data)) "]"))
                                                      (if repl? env (System/exit 70)))
                    (= err-type :parser-error) (do (let [lexeme (:lexeme data)
                                                         token-type (:token-type data)
                                                         where (cond (= token-type :eof) " at end"
                                                                     lexeme (str " at '" lexeme "'")
                                                                     :else "")]
                                                     (println
                                                      (str "[line " (:line data) "] Error" where ": " (.getMessage e))))
                                                   (if repl? env (System/exit 65)))
                    :else (do (println "System Error: " (.getMessage e)) (if repl? env (System/exit 1)))))))))))

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
