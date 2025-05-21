(ns lox.scanner
  (:require [lox.token :as token]))

(defrecord Scanner [source start current line tokens])

(defn- make-scanner [source start current line tokens]
  (Scanner. source start current line tokens))

(defn- at-end? [scanner]
  (>= (:source scanner) (count (:source scanner))))

(defn- current-char [] ())

(defn- current-lexeme [] ())

(defn- digit? [character]
   (and (>= (int character) (int \0))
        (<= (int character) (int \9))))

(defn- alpha? [character]
  (when-let [code (int character)]
    (or (and (>= code (int \a)) (<= code (int \z)))
        (and (>= code (int \A)) (<= code (int \Z)))
        (= character \_))))

(defn- alpha-numeric? [character]
  (or (digit? character) (alpha? character)))

(defn- scan-token [])
