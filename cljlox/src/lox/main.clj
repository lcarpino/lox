(ns lox.main
  (:gen-class)
  (:require [clojure.java.io :as io]
            [lox.core :as core]))

(defn- run-file
  [path]
  (let [file (io/file path)]
    (if (.exists file)
      (let [{:keys [exit-code]} (core/execute (slurp file) (core/create-initial-state))]
        (when exit-code (System/exit exit-code)))
      (println "File not found: " path))))

(defn- run-prompt
  []
  (loop [state (core/create-initial-state)]
    (print "> ")
    (flush)
    (when-some [line (read-line)] (let [new-state (core/execute line state)] (recur (dissoc new-state :exit-code))))))

(defn -main
  [& args]
  (try (let [arglen (count args)]
         (cond (> arglen 1) (do (println "Usage: cljlox [script]") (System/exit 64))
               (= arglen 1) (run-file (first args))
               :else (run-prompt)))
       (catch Exception e (println "System Error: " (ex-message e)) (System/exit 1))))
