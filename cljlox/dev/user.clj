(ns user
  (:require [malli.dev]
            [malli.dev.pretty]))

(defn dev-mode-on! []
  (malli.dev/start! {:report (malli.dev.pretty/thrower)})

(defn dev-mode-off! []
  (malli.dev/stop!)
