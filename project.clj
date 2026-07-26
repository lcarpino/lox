(defproject cljlox "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-zprint "1.3.0"]]
  :source-paths ["cljlox/src"]
  :test-paths ["cljlox/test"]
  :dependencies [[org.clojure/clojure "1.12.4"]]
  :zprint {:old? false
           :width 120
           :vector {:respect-nl? true
                    :wrap? false}
           :map    {:respect-nl? true
                    :justify? true}
           :style :community}
  :main lox.core
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[metosin/malli "0.20.1"]]
                   :source-paths ["cljlox/dev"]}
             :test {:dependencies [[metosin/malli "0.20.1"]]}
             :uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
