(ns lox.token)

(defrecord Token [type lexeme literal line])

(defn make-token
  ([type lexeme line] (make-token type lexeme nil line))
  ([type lexeme literal line] (Token. type lexeme literal line)))
