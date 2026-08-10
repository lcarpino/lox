(ns lox.web
  (:require [lox.core :as core]
            [clojure.string :as str]))

(defonce current-state (atom nil))

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
    (reset! current-state (core/create-initial-state))
    (binding [*print-fn* (fn [& args] (append-output! (str/join " " args) false))
              *print-err-fn* (fn [& args] (append-output! (str/join " " args) true))]
      (swap! current-state (fn [state] (dissoc (core/execute source state) :exit-code))))))

(defn init
  []
  (println "Lox Web Initialized!")
  (let [run-btn (js/document.getElementById "run-btn")
        clear-btn (js/document.getElementById "clear-btn")]
    (.addEventListener run-btn "click" evaluate-code)
    (.addEventListener clear-btn "click" clear-output!)))
