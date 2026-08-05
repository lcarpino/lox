(ns tool.test-harness
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as sh]
            [clojure.set :as set]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.edn :as edn])
  (:gen-class))

(def config (edn/read-string (slurp (io/resource "tool/suites.edn"))))

(def suites
  (loop [remaining (seq (:suites config))
         acc {}]
    (if remaining
      (let [[suite-name suite-data] (first remaining)
            rules (apply merge (:rules suite-data {}) (map #((:vars config) %) (:includes suite-data [])))
            processed-suite (assoc suite-data :rules rules)]
        (recur (next remaining) (assoc acc suite-name processed-suite)))
      acc)))

(def expected-output-pattern #"// expect: ?(.*)")
(def expected-error-pattern #"// (Error.*)")
(def error-line-pattern #"// \[((java|c) )?line (\d+)\] (Error.*)")
(def expected-runtime-error-pattern #"// expect runtime error: (.+)")
(def syntax-error-pattern #"\[.*line (\d+)\] (Error.+)")
(def stack-trace-pattern #"\[line (\d+)\]")
(def non-test-pattern #"// nontest")

(defn parse-test-file
  [path language]
  (let [lines (str/split-lines (slurp path))]
    (loop [remaining lines
           line-num 1
           state {:expected-output        [],
                  :expected-errors        #{},
                  :expected-runtime-error nil,
                  :expected-exit-code     0}]
      (if (empty? remaining)
        state
        (let [line (first remaining)
              next-state
              (cond (re-find non-test-pattern line) :non-test
                    (re-find expected-output-pattern line)
                    (let [[_ out] (re-find expected-output-pattern line)]
                      (update state :expected-output conj {:line line-num, :output out}))
                    (re-find expected-error-pattern line) (let [[_ err] (re-find expected-error-pattern line)]
                                                            (->
                                                              state
                                                              (update :expected-errors conj (str "[" line-num "] " err))
                                                              (assoc :expected-exit-code 65)))
                    (re-find error-line-pattern line) (let [[_ _ lang line-str err] (re-find error-line-pattern line)]
                                                        (if (or (nil? lang) (= lang language))
                                                          (-> state
                                                              (update :expected-errors conj (str "[" line-str "] " err))
                                                              (assoc :expected-exit-code 65))
                                                          state))
                    (re-find expected-runtime-error-pattern line)
                    (let [[_ err] (re-find expected-runtime-error-pattern line)]
                      (-> state
                          (assoc :expected-runtime-error err :runtime-error-line line-num :expected-exit-code 70)))
                    :else state)]
          (if (= next-state :non-test) :non-test (recur (rest remaining) (inc line-num) next-state)))))))

(defn validate-runtime-error
  [{:keys [expected-runtime-error runtime-error-line]} error-lines]
  (cond (< (count error-lines) 2) [(str "Expected runtime error '" expected-runtime-error "' and got none.")]
        (not= (first error-lines) expected-runtime-error)
        [(str "Expected runtime error '" expected-runtime-error "' and got:")
         (first error-lines)]
        :else (let [stack-lines (rest error-lines)
                    match (some #(re-find stack-trace-pattern %) stack-lines)]
                (if-not match
                  (into [(str "Expected stack trace and got:")] stack-lines)
                  (let [stack-line (Integer/parseInt (nth match 1))]
                    (if (not= stack-line runtime-error-line)
                      [(str "Expected runtime error on line " runtime-error-line " but was on line " stack-line ".")]
                      []))))))

(defn validate-compile-errors
  [{:keys [expected-errors]} error-lines]
  (let [result
        (loop [remaining error-lines
               found-errors #{}
               unexpected-count 0
               failures []]
          (if (empty? remaining)
            {:found-errors found-errors, :unexpected-count unexpected-count, :failures failures}
            (let [line (first remaining)]
              (if-let [[_ line-num err] (re-find syntax-error-pattern line)]
                (let [error (str "[" line-num "] " err)]
                  (if (contains? expected-errors error)
                    (recur (rest remaining) (conj found-errors error) unexpected-count failures)
                    (if (< unexpected-count 10)
                      (recur (rest remaining)
                             found-errors
                             (inc unexpected-count)
                             (conj failures "Unexpected error:" line))
                      (recur (rest remaining) found-errors (inc unexpected-count) failures))))
                (if (not (str/blank? line))
                  (if (< unexpected-count 10)
                    (recur (rest remaining)
                           found-errors
                           (inc unexpected-count)
                           (conj failures "Unexpected output on stderr:" line))
                    (recur (rest remaining) found-errors (inc unexpected-count) failures))
                  (recur (rest remaining) found-errors unexpected-count failures))))))

        {:keys [found-errors unexpected-count failures]} result

        failures
        (if (> unexpected-count 10) (conj failures (str "(truncated " (- unexpected-count 10) " more...)")) failures)

        missing-errors (set/difference expected-errors found-errors)]
    (loop [remaining missing-errors
           acc failures]
      (if (empty? remaining)
        acc
        (recur (rest remaining) (conj acc (str "Missing expected error: " (first remaining))))))))

(defn validate-test
  [parsed-expectations exit-code stdout-lines stderr-lines]
  (let [{:keys [expected-output expected-runtime-error expected-exit-code]} parsed-expectations

        failures []

        failures
        (if (not= exit-code expected-exit-code)
          (let [errs (if (> (count stderr-lines) 10) (concat (take 10 stderr-lines) ["(truncated...)"]) stderr-lines)]
            (into failures
                  (concat [(str "Expected return code " expected-exit-code " and got " exit-code ". Stderr:")] errs)))
          failures)

        failures (if expected-runtime-error
                   (into failures (validate-runtime-error parsed-expectations stderr-lines))
                   (into failures (validate-compile-errors parsed-expectations stderr-lines)))

        clean-out
        (if (and (not-empty stdout-lines) (str/blank? (last stdout-lines))) (butlast stdout-lines) stdout-lines)

        failures (loop [idx 0
                        fails failures]
                   (if (>= idx (count clean-out))
                     fails
                     (let [line (nth clean-out idx)]
                       (if (>= idx (count expected-output))
                         (recur (inc idx) (conj fails (str "Got output '" line "' when none was expected.")))
                         (let [expected (nth expected-output idx)]
                           (if (not= (:output expected) line)
                             (recur (inc idx)
                                    (conj fails
                                          (str "Expected output '"
                                               (:output expected)
                                               "' on line "
                                               (:line expected)
                                               " and got '"
                                               line
                                               "'.")))
                             (recur (inc idx) fails)))))))]
    (loop [idx (count clean-out)
           fails failures]
      (if (>= idx (count expected-output))
        fails
        (let [expected (nth expected-output idx)]
          (recur (inc idx)
                 (conj fails
                       (str "Missing expected output '" (:output expected) "' on line " (:line expected) "."))))))))

(defn run-interpreter
  [interpreter path]
  (let [cmd (concat (str/split interpreter #" ") [(str path)])
        result (apply sh/sh cmd)]
    {:exit (:exit result),
     :out  (str/split-lines (:out result)),
     :err  (str/split-lines (:err result))}))

(defn find-lox-files
  [dir]
  (->> (io/file dir)
       file-seq
       (filter #(.isFile %))
       (filter #(str/ends-with? (.getName %) ".lox"))
       (map #(.getPath %))
       (sort)))

(defn determine-suite-state
  [suite-rules path-str]
  (let [parts (str/split path-str #"/")]
    (loop [remaining parts
           subpath ""
           state nil]
      (if (empty? remaining)
        state
        (let [part (first remaining)
              new-subpath (if (empty? subpath) part (str subpath "/" part))
              new-state (get suite-rules new-subpath state)]
          (recur (rest remaining) new-subpath new-state))))))

(def GREEN "\u001b[32m")
(def RED "\u001b[31m")
(def YELLOW "\u001b[33m")
(def GRAY "\u001b[90m")
(def PINK "\u001b[35m")
(def RESET "\u001b[0m")
(def CLEAR-LINE "\u001b[2K\r")

(defn render-progress-bar
  [passed failed skipped path]
  (print CLEAR-LINE)
  (print
   (str "Passed: "
        GREEN
        passed
        RESET
        " "
        "Failed: "
        RED
        failed
        RESET
        " "
        "Skipped: "
        YELLOW
        skipped
        RESET
        " " GRAY
        "(" path
        ")" RESET))
  (flush))

(defn render-failures
  [path failures]
  (print CLEAR-LINE)
  (println (str RED "FAIL" RESET " " path))
  (println)
  (doseq [f failures] (println (str "     " PINK f RESET)))
  (println))

(def cli-options
  [["-i" "--interpreter PATH" "Path to the interpreter executable"]
   ["-d" "--dir PATH" "Path to the test directory"]
   ["-s" "--suite NAME" "Name of the test suite (e.g. jlox, clox)"]
   ["-h" "--help" "Show help message"]])

(defn usage
  [options-summary]
  (->> ["Lox Test Runner"
        ""
        "Usage: test-harness [options]"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(defn -main
  [& args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (cond (:help options) (do (println (usage summary)) (System/exit 0))
          errors (do (println "Errors:") (doseq [e errors] (println e)) (System/exit 1))
          (not (and (:interpreter options) (:dir options) (:suite options))) (do (println (usage summary))
                                                                                 (System/exit 1)))
    (let [{:keys [interpreter dir suite]} options
          suite-info (get suites suite)
          test-files (find-lox-files dir)]
      (when-not suite-info (binding [*out* *err*] (println "Unknown suite:" suite)) (System/exit 1))
      (let [suite-rules (:rules suite-info)
            suite-lang (:language suite-info)]
        (loop [files test-files
               passed 0
               failed 0
               skipped 0
               expectations 0]
          (if (empty? files)
            (do (print CLEAR-LINE)
                (if (= failed 0)
                  (println (str "All " GREEN passed RESET " tests passed (" expectations " expectations)."))
                  (println (str GREEN passed RESET " tests passed. " RED failed RESET " tests failed.")))
                (if (> failed 0) (System/exit 1) (System/exit 0)))
            (let [path-str (str/replace (first files) #"\\" "/")
                  state (if (str/includes? path-str "benchmark") :skip (determine-suite-state suite-rules path-str))]
              (if (= state :skip)
                (recur (rest files) passed failed (inc skipped) expectations)
                (let [parsed (parse-test-file path-str suite-lang)]
                  (if (= parsed :non-test)
                    (recur (rest files) passed failed (inc skipped) expectations)
                    (do (render-progress-bar passed failed skipped path-str)
                        (let [result (run-interpreter interpreter path-str)
                              failures (validate-test parsed (:exit result) (:out result) (:err result))
                              new-expectations (+ expectations
                                                  (count (:expected-output parsed))
                                                  (count (:expected-errors parsed))
                                                  (if (:expected-runtime-error parsed) 1 0))]
                          (if (empty? failures)
                            (recur (rest files) (inc passed) failed skipped new-expectations)
                            (do (render-failures path-str failures)
                                (recur (rest files) passed (inc failed) skipped new-expectations)))))))))))))))
