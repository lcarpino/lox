(ns lox.main
  (:gen-class)
  (:require [clojure.java.io :as io]
            [lox.memory :as memory]
            [lox.native :as native]
            [lox.core :as core]))

(defn- run-file
  [path]
  (let [file (io/file path)]
    (if (.exists file)
      (do (memory/empty-store!)
          (let [{:keys [exit-code]} (core/execute (slurp file) (native/create-global-env))]
            (when exit-code (System/exit exit-code))))
      (println "File not found: " path))))

(defn- run-prompt
  []
  (memory/empty-store!)
  (loop [env (native/create-global-env)]
    (print "> ")
    (flush)
    (when-some [line (read-line)] (let [{:keys [env]} (core/execute line env)] (recur env)))))

(defn -main
  [& args]
  (try (let [arglen (count args)]
         (cond (> arglen 1) (do (println "Usage: cljlox [script]") (System/exit 64))
               (= arglen 1) (run-file (first args))
               :else (run-prompt)))
       (catch Exception e (println (format "Fatal error: %s" (ex-message e))))))
