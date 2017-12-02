(ns net.lfn3.undertaker.core-test-unstrumented
  (:require [clojure.test :refer [deftest is] :as t]
            [net.lfn3.undertaker.core :as undertaker]
            [clojure.test :as t]))

(deftest can-handle-throws
  (let [ex (ex-info "An exception!" {})
        result (undertaker/run-prop {} (fn [] (throw ex)))]
    (is (false? (get-in result [::undertaker/initial-results ::undertaker/result])))
    (is (= ex (get-in result [::undertaker/initial-results ::undertaker/cause])))))

(deftest should-throw-on-nested-props
  (is (->> #(undertaker/run-prop {} (constantly true))
           (undertaker/run-prop {})
           ::undertaker/initial-results
           ::undertaker/cause
           (instance? IllegalStateException))))
