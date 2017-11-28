(defproject net.lfn3/undertaker "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies []

  :plugins [[lein-jmh "0.2.2"]]

  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :test-paths ["src/test/clojure" "src/test/benchmarks"]
  :target-path "target/%s"

  :profiles {:provided  {:dependencies [[org.clojure/clojure "1.9.0-RC2"]]}
             :dev       {:source-paths      ["src/specs/clojure"]
                         :jvm-opts          ["-Dundertaker.debug=true"]
                         :dependencies      [[orchestra "2017.11.12-1"]
                                             [org.clojure/test.check "0.9.0"]]
                         :aot               [net.lfn3.undertaker.junit.source-rule]}

             :specs-jar {:source-paths      ^:replace ["src/specs/clojure"]
                         :java-source-paths ^:replace []
                         :jar-name          "undertaker-specs.jar"}

             :test      {:source-paths ["src/specs/clojure"]
                         :jvm-opts     ["-Dundertaker.debug=true" "-Xmx8g"]}
             :jmh       {:jvm-opts ["-Dundertaker.debug=false"]}})
