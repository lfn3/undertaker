(defproject undertaker "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/spec.alpha "0.1.123"]
                 [orchestra "0.3.0"]
                 [org.clojure/test.check "0.9.0"]
                 [criterium "0.4.4"]]
  :main ^:skip-aot undertaker.core
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
