(ns lox.scanner)

(def keywords
  {"and" :token/and,
   "class" :token/class,
   "else" :token/else,
   "false" :token/false,
   "for" :token/for,
   "fun" :token/fun,
   "if" :token/if,
   "nil" :token/nil,
   "or" :token/or,
   "print" :token/print,
   "return" :token/return,
   "super" :token/super,
   "this" :token/this,
   "true" :token/true,
   "var" :token/var,
   "while" :token/while})

(defn- at-end? [scanner] (>= (::current scanner) (count (::source scanner))))

(defn- advance [scanner] (update scanner ::current inc))

(defn- current-character [scanner] (nth (::source scanner) (::current scanner)))

(defn- current-lexeme [scanner] (subs (::source scanner) (::start scanner) (::current scanner)))

;; (defn- peek [scanner]
;;   ())

(defn- peek-next
  [scanner]
  (let [next (inc (::current scanner)) source (::source scanner)] (when (< next (count source)) (nth source next))))

(defn- add-token
  ([scanner token-type] (add-token scanner token-type nil))
  ([scanner token-type literal]
   (update scanner
           ::tokens
           conj
           {::type token-type, ::lexeme (current-lexeme scanner), ::literal literal, ::line (::line scanner)})))

(defn- add-error [scanner message] (update scanner ::errors conj {::line scanner, ::message message}))

(defn- match [scanner expected] (and (not (at-end? scanner)) (= (current-character scanner) expected)))

(defn- digit? [character] (and (>= (int character) (int \0)) (<= (int character) (int \9))))

(defn- alpha?
  [character]
  (when-let [code (int character)]
    (or (and (>= code (int \a)) (<= code (int \z))) (and (>= code (int \A)) (<= code (int \Z))) (= character \_))))

(defn- alpha-numeric? [character] (or (digit? character) (alpha? character)))

(defn- skip-comment
  [scanner]
  (if (or (at-end? scanner) (= (current-character scanner) \newline)) scanner (recur (advance scanner))))

(defn- add-identifier
  [scanner]
  (if (or (at-end? scanner) (not (alpha-numeric? (current-character scanner))))
    (add-token scanner (get keywords (current-lexeme scanner) :token/identifier))
    (recur (advance scanner))))

(defn- add-number
  [scanner]
  (if (or (at-end? scanner) (not (digit? (current-character scanner))))
    (if (and (match scanner \.) (digit? (peek-next scanner)))
      (recur (advance scanner))
      (add-token scanner :token/number (Double/parseDouble (current-lexeme scanner))))
    (recur (advance scanner))))

(defn- add-string
  [scanner]
  (if (at-end? scanner)
    (add-error scanner "Unterminated string.")
    (if (match scanner \")
      (let [scanner (advance scanner)]
        (add-token scanner :token/string (subs (::source scanner) (inc (::start scanner)) (dec (::current scanner)))))
      (recur (advance (if (= (current-character scanner) \newline) (update scanner ::line inc) scanner))))))

(defn- scan-token
  [scanner]
  (let [character (current-character scanner)
        scanner (advance scanner)]
    (case character
      \( (add-token scanner :token/lparam)
      \) (add-token scanner :token/rparam)
      \{ (add-token scanner :token/lbrace)
      \} (add-token scanner :token/rbrace)
      \, (add-token scanner :token/comma)
      \. (add-token scanner :token/dot)
      \- (add-token scanner :token/minus)
      \+ (add-token scanner :token/plus)
      \; (add-token scanner :token/semicolon)
      \* (add-token scanner :token/star)
      \! (if (match scanner \=) (add-token (advance scanner) :token/bang-equal) (add-token scanner :token/bang))
      \= (if (match scanner \=) (add-token (advance scanner) :token/equal-equal) (add-token scanner :token/equal))
      \< (if (match scanner \=) (add-token (advance scanner) :token/less-equal) (add-token scanner :token/less))
      \> (if (match scanner \=) (add-token (advance scanner) :token/greater-equal) (add-token scanner :token/greater))
      \/ (if (match scanner \/) (skip-comment (advance scanner)) (add-token scanner :token/slash))
      (\return \space \tab) scanner
      \newline (update scanner ::line inc)
      \" (add-string scanner)
      (cond (alpha? character) (add-identifier scanner)
            (digit? character) (add-number scanner)
            :else (add-error scanner "Unexpected character.")))))

(defn- next-token [scanner] (assoc scanner ::start (::current scanner)))

(defn scan
  [source]
  (loop [scanner {::source source, ::errors [], ::start 0, ::current 0, ::line 1, ::tokens []}]
    (if (at-end? scanner)
      (let [scanner (add-token scanner :token/eof)] [(::tokens scanner) (::errors scanner)])
      (recur (-> scanner
                 (scan-token)
                 (next-token))))))
