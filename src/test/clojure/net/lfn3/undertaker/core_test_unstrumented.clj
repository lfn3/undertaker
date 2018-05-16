(ns net.lfn3.undertaker.core-test-unstrumented
  (:require [clojure.test :refer [deftest is] :as t]
            [net.lfn3.undertaker.core :as undertaker]))

(t/use-fixtures :once #(with-redefs [undertaker/print-initial-failure (constantly nil)]
                         (%1)))

(deftest can-handle-throws
  (let [ex (ex-info "An exception!" {})
        result (undertaker/run-prop {::undertaker/test-name "can-handle-throws"} (fn [] (throw ex)))]
    (is (false? (get-in result [::undertaker/initial-results ::undertaker/result])))
    (is (= ex (get-in result [::undertaker/initial-results ::undertaker/cause])))))

(deftest should-only-run-once-since-source-is-unused
  (let [result (->> (fn [])
                    (undertaker/run-prop {::undertaker/test-name "should-only-run-once-since-source-is-unused"}))]
    (is (= 1 (::undertaker/iterations-run result)))))