(ns lox.evaluator)

(defn- truthy? [val]
  (not (or (nil? val) (false? val))))

(defmulti evaluate :type)

(defmethod evaluate :literal [node]
  (:value node))

(defmethod evaluate :grouping [node]
  (evaluate (:expression node)))

(defmethod evaluate :unary [node]
  (let [right   (evaluate (:right node))
        op-type (get-in node [:op :type])]
    (cond
      (= op-type :minus) (- right)
      (= op-type :bang)  (not (truthy? right))
      :else (throw (ex-info "Unknown unary operator" {:node node})))))

(defmethod evaluate :binary [node]
  (let [left    (evaluate (:left node))
        right   (evaluate (:right node))
        op-type (get-in node [:op :type])]
    (cond
      (= op-type :minus)         (- left right)
      (= op-type :slash)         (/ left right)
      (= op-type :star)          (* left right)
      (= op-type :greater)       (> left right)
      (= op-type :greater-equal) (>= left right)
      (= op-type :less)          (< left right)
      (= op-type :less-equal)    (<= left right)
      (= op-type :bang-equal)    (not= left right)
      (= op-type :equal-equal)   (= left right)
      (= op-type :plus)
      (if (and (string? left) (string? right))
        (str left right)
        (+ left right))
      :else (throw (ex-info "Unknown binary operator" {:node node})))))
