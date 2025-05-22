(ns lox.core
  (:require [lox.scanner :as scanner]))

(defn- run [source]
  (doseq [tokens (::tokens (scanner/scan source))]
    (println tokens)))

(defn- run-file [& args])

(defn- run-prompt []
  (loop []
    (print "> ")
    (flush)
    (when-some [line (read-line)]
      (println (run line))
      (recur))))

(defn -main [& args]
  (try
    (let [arglen (count args)]
      (cond
        (> arglen 1) (println "Usage: cljlox [script]")
        (= arglen 1) (run-file (first args))
        :else
        (run-prompt)))
    (catch Exception e
      (println  (format "Fatal error: %s" (ex-message e))))))

