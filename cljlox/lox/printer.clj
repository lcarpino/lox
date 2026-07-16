(ns lox.printer
  (:require [clojure.string :as str]
            [lox.ast :as ast]))

(def FormatSchema [:enum :sexp :lox])

(defmulti print-ast
  {:malli/schema [:=> [:cat FormatSchema [:or ast/ExprSchema ast/StmtSchema]] :string]}
  (fn [fmt node] [fmt (:type node)]))

(defn print-program
  {:malli/schema [:=> [:cat FormatSchema [:sequential ast/StmtSchema]] :string]}
  [fmt statements]
  (loop [stmts statements
         acc []]
    (if (empty? stmts) (str/join "\n" acc) (recur (rest stmts) (conj acc (print-ast fmt (first stmts)))))))

(defmethod print-ast [:sexp :literal]
  [_ node]
  (let [val (:value node)]
    (cond (nil? val) "nil"
          (string? val) (str "\"" val "\"")
          :else (str val))))

(defmethod print-ast [:lox :literal] [fmt node] (print-ast :sexp node))

(defmethod print-ast [:sexp :variable] [_ node] (:lexeme (:name node)))
(defmethod print-ast [:lox :variable] [_ node] (:lexeme (:name node)))

(defmethod print-ast [:sexp :grouping] [fmt node] (str "(group " (print-ast fmt (:expression node)) ")"))

(defmethod print-ast [:lox :grouping] [fmt node] (str "(" (print-ast fmt (:expression node)) ")"))

(defmethod print-ast [:sexp :unary] [fmt node] (str "(" (:lexeme (:op node)) " " (print-ast fmt (:right node)) ")"))

(defmethod print-ast [:lox :unary] [fmt node] (str (:lexeme (:op node)) (print-ast fmt (:right node))))

(defmethod print-ast [:sexp :binary]
  [fmt node]
  (str "(" (:lexeme (:op node)) " " (print-ast fmt (:left node)) " " (print-ast fmt (:right node)) ")"))

(defmethod print-ast [:lox :binary]
  [fmt node]
  (str (print-ast fmt (:left node)) " " (:lexeme (:op node)) " " (print-ast fmt (:right node))))

(defmethod print-ast [:sexp :assign]
  [fmt node]
  (str "(= " (:lexeme (:name node)) " " (print-ast fmt (:value node)) ")"))

(defmethod print-ast [:lox :assign] [fmt node] (str (:lexeme (:name node)) " = " (print-ast fmt (:value node))))

(defmethod print-ast [:sexp :expr] [fmt node] (print-ast fmt (:expression node)))

(defmethod print-ast [:lox :expr] [fmt node] (str (print-ast fmt (:expression node)) ";"))

(defmethod print-ast [:sexp :print] [fmt node] (str "(print " (print-ast fmt (:expression node)) ")"))

(defmethod print-ast [:lox :print] [fmt node] (str "print " (print-ast fmt (:expression node)) ";"))

(defmethod print-ast [:sexp :var-stmt]
  [fmt node]
  (let [init (or (:initializer node) (:initialiser node))]
    (if init
      (str "(var " (:lexeme (:name node)) " " (print-ast fmt init) ")")
      (str "(var " (:lexeme (:name node)) ")"))))

(defmethod print-ast [:lox :var-stmt]
  [fmt node]
  (let [init (or (:initializer node) (:initialiser node))]
    (if init
      (str "var " (:lexeme (:name node)) " = " (print-ast fmt init) ";")
      (str "var " (:lexeme (:name node)) ";"))))

(defmethod print-ast [:sexp :block]
  [fmt node]
  (let [stmts-str (map #(print-ast fmt %) (:statements node))] (str "(block " (str/join " " stmts-str) ")")))

(defmethod print-ast [:lox :block]
  [fmt node]
  (let [stmts-str (map #(print-ast fmt %) (:statements node))] (str "{ " (str/join " " stmts-str) " }")))

(defmethod print-ast :default [fmt node] (str "<unknown-node: " (:type node) " for format " fmt ">"))
