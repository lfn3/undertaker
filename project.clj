(defproject undertaker "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/spec.alpha "0.1.123"]
                 [orchestra "0.3.0"]
                 [org.clojure/test.check "0.9.0"]
                 [criterium "0.4.4"]
                 [junit/junit "4.12"]]
  :source-paths ["src/main/clojure"]
  :prep-tasks ^:replace [["with-profile" "java-interfaces" "javac"]
                         "compile"
                         "javac"]
  :java-source-paths ["src/main/java" "src/test/java"]
  :test-paths ["src/test/clojure"]
  :target-path "target/%s"
  :aot [undertaker.source-rule]
  :profiles {:uberjar {:aot :all}
             :java-interfaces {:java-source-paths ^:replace ["src/main/java"]}
            })
