(ns lox.parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [lox.parser :as parser]
            [lox.scanner :as scanner]
            [malli.generator :as mg]))

(deftest test-fuzz-parser
  (testing "Parser handles random token streams without crashing"
    (let [random-token-streams (mg/sample scanner/ScannerOutputSchema {:size 100})]
      (doseq [tokens random-token-streams]
        (try (parser/parse {:tokens tokens})
             (is true)
             (catch Exception e
               (let [data (ex-data e)]
                 (is (contains? data :line)
                     (str "Parser threw a JVM exception instead of a syntax error! \n"
                          "Message: "
                          (.getMessage e)
                          "\n"
                          "Tokens: "
                          (pr-str tokens))))))))))
