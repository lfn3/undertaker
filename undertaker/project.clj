(def version "0.1.0-SNAPSHOT")

(defproject com.lmax/undertaker version
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies []

  :plugins [[lein-jmh "0.2.2"]]

  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :test-paths ["src/test/clojure" "src/test/benchmarks"]
  :target-path "target/%s"

  :profiles {:dev {:jvm-opts ["-Dundertaker.debug=true"]
                   :dependencies [[org.clojure/clojure "1.9.0-RC1"]

                                  [orchestra "2017.11.12-1"]
                                  [org.clojure/test.check "0.9.0"]
                                  [criterium "0.4.4"]]}
             :test {:jvm-opts ["-Dundertaker.debug=true" "-Xmx8g"]}
             :jmh {:jvm-opts ["-Dundertaker.debug=false"]}})
