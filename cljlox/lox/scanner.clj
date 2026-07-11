(ns lox.scanner)

(def TokenSchema [:map [:type :keyword] [:lexeme :string] [:line :int] [:literal {:optional true} :any]])

(def ScannerStateSchema [:map [:chars [:sequential :char]] [:line :int]])

(def ScannerOutputSchema [:sequential TokenSchema])

(def keywords
  {"and" :and,
   "class" :class,
   "else" :else,
   "false" :false,
   "for" :for,
   "fun" :fun,
   "if" :if,
   "nil" :nil,
   "or" :or,
   "print" :print,
   "return" :return,
   "super" :super,
   "this" :this,
   "true" :true,
   "var" :var,
   "while" :while})

(defn- digit? [c] (and c (Character/isDigit c)))

(defn- alpha? [c] (and c (or (Character/isLetter c) (= c \_))))

(defn- alpha-numeric? [c] (or (alpha? c) (digit? c)))

(defn- scan-string
  {:malli/schema [:=> [:cat ScannerStateSchema] [:tuple TokenSchema ScannerStateSchema]]}
  [initial-state]
  (loop [state initial-state
         acc []]
    (let [c (first (:chars state))]
      (cond (nil? c) (let [lexeme (apply str acc)]
                       [{:type :error, :lexeme lexeme, :line (:line state), :literal "Unterminated string."}
                        (assoc state :chars '())])
            (= c \") (let [lexeme (apply str acc)]
                       [{:type :string, :lexeme (str "\"" lexeme "\""), :literal lexeme, :line (:line state)}
                        (update state :chars rest)])
            (= c \newline) (recur (-> state
                                      (update :chars rest)
                                      (update :line inc))
                                  (conj acc c))
            :else (recur (update state :chars rest) (conj acc c))))))

(defn- scan-number
  {:malli/schema [:=> [:cat ScannerStateSchema] [:tuple TokenSchema ScannerStateSchema]]}
  [initial-state]
  (loop [state initial-state
         acc []]
    (let [c (first (:chars state))]
      (if (or (digit? c) (= c \.))
        (recur (update state :chars rest) (conj acc c))
        (let [lexeme (apply str acc)
              value (Double/parseDouble lexeme)]
          [{:type :number, :lexeme lexeme, :literal value, :line (:line state)} state])))))

(defn- scan-identifier
  {:malli/schema [:=> [:cat ScannerStateSchema] [:tuple TokenSchema ScannerStateSchema]]}
  [initial-state]
  (loop [state initial-state
         acc []]
    (let [c (first (:chars state))]
      (if (alpha-numeric? c)
        (recur (update state :chars rest) (conj acc c))
        (let [lexeme (apply str acc)
              type (get keywords lexeme :identifier)]
          [{:type type, :lexeme lexeme, :line (:line state)} state])))))


(defn scan-token
  {:malli/schema [:=> [:cat ScannerStateSchema] [:tuple TokenSchema ScannerStateSchema]]}
  [initial-state]
  (loop [state initial-state]
    (let [chars (:chars state)
          line (:line state)
          c (first chars)]
      (if (nil? c)
        [{:type :eof, :lexeme "", :line line} state]
        (cond
          ;; --- single character operators ---
          (= c \() [{:type :lparen, :lexeme "(", :line line} (update state :chars rest)]
          (= c \)) [{:type :rparen, :lexeme ")", :line line} (update state :chars rest)]
          (= c \{) [{:type :lbrace, :lexeme "{", :line line} (update state :chars rest)]
          (= c \}) [{:type :rbrace, :lexeme "}", :line line} (update state :chars rest)]
          (= c \,) [{:type :comma, :lexeme ",", :line line} (update state :chars rest)]
          (= c \.) [{:type :dot, :lexeme ".", :line line} (update state :chars rest)]
          (= c \-) [{:type :minus, :lexeme "-", :line line} (update state :chars rest)]
          (= c \+) [{:type :plus, :lexeme "+", :line line} (update state :chars rest)]
          (= c \;) [{:type :semicolon, :lexeme ";", :line line} (update state :chars rest)]
          (= c \*) [{:type :star, :lexeme "*", :line line} (update state :chars rest)]
          ;; --- two character operators ---
          (= c \!) (if (= (second chars) \=)
                     [{:type :bang-equal, :lexeme "!=", :line line} (update state :chars #(drop 2 %))]
                     [{:type :bang, :lexeme "!", :line line} (update state :chars rest)])
          (= c \=) (if (= (second chars) \=)
                     [{:type :equal-equal, :lexeme "==", :line line} (update state :chars #(drop 2 %))]
                     [{:type :equal, :lexeme "=", :line line} (update state :chars rest)])
          (= c \<) (if (= (second chars) \=)
                     [{:type :less-equal, :lexeme "<=", :line line} (update state :chars #(drop 2 %))]
                     [{:type :less, :lexeme "<", :line line} (update state :chars rest)])
          (= c \>) (if (= (second chars) \=)
                     [{:type :greater-equal, :lexeme ">=", :line line} (update state :chars #(drop 2 %))]
                     [{:type :greater, :lexeme ">", :line line} (update state :chars rest)])
          ;; --- comments ---
          (= c \/) (if (= (second chars) \/)
                     (let [comment-chars (take-while #(not= % \newline) chars)]
                       (recur (update state :chars #(drop (count comment-chars) %))))
                     [{:type :slash, :lexeme "/", :line line} (update state :chars rest)])
          ;; --- whitespace ---
          (contains? #{\space \return \tab} c) (recur (update state :chars rest))
          (= c \newline) (recur (-> state
                                    (update :chars rest)
                                    (update :line inc)))
          ;; --- literals ---
          (= c \") (scan-string state)
          (alpha? c) (scan-identifier state)
          (digit? c) (scan-number state)
          ;; --- fallback ---
          :else [{:type :error, :lexeme (str c), :line line, :literal "Unexpected character."}
                 (update state :chars rest)])))))

(defn scan
  {:malli/schema [:=> [:cat :string] ScannerOutputSchema]}
  [source]
  (loop [state {:chars (seq source), :line 1}
         tokens []]
    (let [[token next-state] (scan-token state)]
      (if (= (:type token) :eof) (conj tokens token) (recur next-state (conj tokens token))))))
