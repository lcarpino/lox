(ns lox.web
  (:require [lox.memory :as memory]
            [lox.native :as native]
            [lox.core :as core]
            [clojure.string :as str]))

(defonce current-env (atom nil))

(defn- append-output!
  [text is-error?]
  (let [output-el (js/document.getElementById "console-output")
        span (js/document.createElement "span")]
    (set! (.-textContent span) text)
    (when is-error? (set! (.-className span) "error-text"))
    (.appendChild output-el span)
    (set! (.-scrollTop output-el) (.-scrollHeight output-el))))

(defn- clear-output!
  []
  (let [output-el (js/document.getElementById "console-output")] (set! (.-innerHTML output-el) "")))

(defn- evaluate-code
  []
  (clear-output!)
  (let [source (.-value (js/document.getElementById "code-editor"))]
    (memory/empty-store!)
    (reset! current-env (native/create-global-env))
    (binding [*print-fn* (fn [& args] (append-output! (str/join " " args) false))
              *print-err-fn* (fn [& args] (append-output! (str/join " " args) true))]
      (swap! current-env (fn [env] (:env (core/execute source env)))))))

(defn init
  []
  (println "Lox Web Initialized!")
  (let [run-btn (js/document.getElementById "run-btn")
        clear-btn (js/document.getElementById "clear-btn")]
    (.addEventListener run-btn "click" evaluate-code)
    (.addEventListener clear-btn "click" clear-output!)))
