(def version "0.1.0-SNAPSHOT")

(defproject com.lmax/undertaker-junit version
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[com.lmax/undertaker ~version]]

  :plugins [[lein-junit "1.1.8"]
            [lein-jmh "0.2.2"]]
  :junit ["src/test/java"]

  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :test-paths ["src/test/java"]
  :target-path "target/%s"

  :profiles {:provided {:dependencies [[org.clojure/clojure "1.9.0-RC1"]
                                       [junit/junit "4.12"]
                                       [org.clojure/test.check "0.9.0"]]} ;need to pull out specs...
             :dev      {:jvm-opts     ["-Dundertaker.debug=true"]
                        :dependencies [[orchestra "2017.11.12-1"]]}
             :jmh      {:jvm-opts ["-Dundertaker.debug=false"]}}

  :aot [com.lmax.undertaker.junit.source-rule])
