(ns lox.main
  (:require [lox.memory :as memory]
            [lox.native :as native]
            [lox.core :as core]
            ["fs" :as fs]
            ["readline" :as readline]))

(defn- run-file
  [path]
  (if (fs/existsSync path)
    (do (memory/empty-store!)
        (let [{:keys [exit-code]} (core/execute (fs/readFileSync path "utf-8") (native/create-global-env))]
          (when exit-code (js/process.exit exit-code))))
    (println "File not found: " path)))

(defn- repl-loop
  [rl env]
  (.question rl "> " (fn [line] (let [result (core/execute line env)] (repl-loop rl (:env result))))))

(defn- run-prompt
  []
  (memory/empty-store!)
  (let [rl (readline/createInterface #js {:input  js/process.stdin,
                                          :output js/process.stdout})]
    (repl-loop rl (native/create-global-env))
    (.on rl "close" (fn [] (println) (js/process.exit 0)))))

(defn main
  [& args]
  (let [args (js/process.argv.slice 2)
        arglen (.-length args)]
    (try (cond (> arglen 1) (do (println "Usage: cljslox [script]") (js/process.exit 64))
               (= arglen 1) (run-file (aget args 0))
               :else (run-prompt))
         (catch js/Error e (println (str "Fatal error: " (.-message e)))))))
