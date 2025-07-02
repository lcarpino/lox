(ns lox.core
  (:require [lox.scanner :as scanner]))

(defn- run [source]
  (let [[tokens errors] (scanner/scan source)]
    (doseq [token tokens]
      (println token))))

(defn- run-file [& args])

(defn- run-prompt []
  (loop []
    (print "> ")
    (flush)
    (when-some [line (read-line)]
      (run line)
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
