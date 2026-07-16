(ns lox.scanner-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.generator :as mg]
            [lox.scanner :as scanner]))

(deftest test-fuzz-scanner
  (testing "Scanner never crashes and always returns valid Tokens on random strings"
    (let [random-strings (mg/sample :string {:size 100})]
      (doseq [source random-strings]
        (let [tokens (scanner/scan source)]
          (is (m/validate scanner/ScannerOutputSchema tokens)
              (str "Scanner output failed validation for source: " (pr-str source))))))))
