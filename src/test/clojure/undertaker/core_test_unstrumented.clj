(ns undertaker.core-test-unstrumented
  (:require [clojure.test :refer :all]
            [undertaker.core :as undertaker]
            [clojure.test :as t]))

(t/use-fixtures :each undertaker/fixture)

(deftest can-handle-throws
    (let [ex (ex-info "An exception!" {})
          result (undertaker/run-prop {} (fn [source] (throw ex)))]
        (is (false? (::undertaker/result result)))
        (is (= ex (::undertaker/cause result)))))

