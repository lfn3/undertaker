(ns undertaker.benchmarks
  (:require [undertaker.core :as undertaker]
            [criterium.core :as criterium]
            [clojure.test :as t :refer [deftest]]
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [clojure.spec.test.alpha :as s.test]
            [undertaker.source.forgetful :as source.forgetful]))

(deftest benchmark-byte-gen
  (with-bindings {#'undertaker/*source* (source.forgetful/make-source (System/nanoTime))}
    (criterium/quick-bench
      (undertaker/byte))))
