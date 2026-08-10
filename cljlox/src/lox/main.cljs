(ns lox.main
  (:require [lox.core :as core]
            ["fs" :as fs]
            ["readline" :as readline]))

(defn- run-file
  [path]
  (if (fs/existsSync path)
    (let [{:keys [exit-code]} (core/execute (fs/readFileSync path "utf-8") (core/create-initial-state))]
      (when exit-code (js/process.exit exit-code)))
    (println "File not found: " path)))

(defn- repl-loop
  [rl state]
  (.question rl
             "> "
             (fn [line] (let [new-state (core/execute line state)] (repl-loop rl (dissoc new-state :exit-code))))))

(defn- run-prompt
  []
  (let [rl (readline/createInterface #js {:input  js/process.stdin,
                                          :output js/process.stdout})]
    (repl-loop rl (core/create-initial-state))
    (.on rl "close" (fn [] (println) (js/process.exit 0)))))

(defn main
  [& args]
  (try (let [arglen (count args)]
         (cond (> arglen 1) (do (println "Usage: cljslox [script]") (js/process.exit 64))
               (= arglen 1) (run-file (first args))
               :else (run-prompt)))
       (catch js/Error e (println (str "Fatal error: " (.-message e))))))
