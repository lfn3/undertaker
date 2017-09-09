(def version "0.1.0-SNAPSHOT")

(defproject com.lmax/undertaker version
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha20"]
                 [org.clojure/spec.alpha "0.1.123"]]

  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :test-paths ["src/test/clojure"]
  :target-path "target/%s"

  :profiles {:dev {:dependencies [[orchestra "0.3.0"]
                                   [org.clojure/test.check "0.9.0"]
                                   [criterium "0.4.4"]]}
             :benchmarks {:test-paths ^:replace ["src/test/benchmarks"]
                          :dependencies [[criterium "0.4.4"]
                                         [org.clojure/test.check "0.9.0"]]}}
  :aliases {"bench" ["with-profile" "benchmarks" "test"]})
