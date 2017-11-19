(def version "0.1.0-SNAPSHOT")

(defproject com.lmax/undertaker-junit version
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-RC1"]
                 [orchestra "2017.11.12-1"]
                 [org.clojure/test.check "0.9.0"]
                 [criterium "0.4.4"]
                 [junit/junit "4.12"]
                 [com.lmax/undertaker ~version]]

  :plugins [[lein-junit "1.1.8"]]
  :junit ["src/test/java"]

  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :test-paths ["src/test/java"]
  :target-path "target/%s"

  :aot [com.lmax.undertaker.junit.source-rule])
