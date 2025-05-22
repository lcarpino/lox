(ns lox.scanner)

(defn- at-end? [scanner]
  (>= (::current scanner) (count (::source scanner))))

(defn- advance [scanner]
  (update scanner ::current inc))

(defn- current-character [scanner]
  (nth (::source scanner) (::current scanner)))

(defn- current-lexeme [scanner]
  (subs (::source scanner) (::start scanner) (::current scanner)))

(defn- peek [scanner]
  ())

(defn- peek-next [scanner]
  (let [next (inc (::current scanner))
        source (::source scanner)]
    (when (< next (count source))
      (nth source next))))

(defn- add-token
  ([scanner token-type]
   (add-token scanner token-type nil))
  ([scanner token-type literal]
   (do (println scanner)
       (update scanner ::tokens conj {::type token-type
                                      ::lexeme (current-lexeme scanner)
                                      ::literal literal
                                      ::line (::line scanner)}))))

(defn- match [scanner expected]
  (and (not (at-end? scanner)) (= (current-character scanner) expected)))

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

(defn- skip-comment [scanner] ())

(defn- identifier [scanner] ())

(defn- number [scanner]
  (if (or (at-end? scanner) (not (digit? (current-character scanner))))
    (if (and (match scanner \.) (digit? (peek-next scanner)))
      (recur (advance scanner))
      scanner)
  (recur (advance scanner))))

(defn- string [scanner] ())

(defn- scan-token [scanner]
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
      \! (if (match scanner \=)
           (add-token (advance scanner) :token/bang-equal)
           (add-token scanner :token/bang))
      \= (if (match scanner \=)
           (add-token (advance scanner) :token/equal-equal)
           (add-token scanner :token/equal))
      \< (if (match scanner \=)
           (add-token (advance scanner) :token/less-equal)
           (add-token scanner :token/less))
      \> (if (match scanner \=)
           (add-token (advance scanner) :token/greater-equal)
           (add-token scanner :token/greater))
      \/ (if (match scanner \/)
           (skip-comment (advance scanner))
           (add-token scanner :token/slash))
      (\return \space \tab) scanner
      \newline (update scanner ::line inc)
      \" (add-token scanner :token/string (string scanner))

      (cond
        (alpha? character) (add-token scanner :token/identifier)
        (digit? character) (let [scanner (number scanner)] (add-token scanner :token/number (Double/parseDouble (current-lexeme scanner))))
        :else ()))))

(defn- next-token [scanner]
  (assoc scanner ::start (::current scanner)))

(defn scan [source]
  (loop [scanner {::source source
                  ::errors []
                  ::start 0
                  ::current 0
                  ::line 1
                  ::tokens []}]
    (if (at-end? scanner)
      (let [scanner (add-token scanner :token/eof)]
        [(::tokens scanner) (::errors scanner)])
      (recur (-> scanner
                 (scan-token)
                 (next-token))))))
