(ns undertaker.benchmarks
  (:require [undertaker.core :as undertaker]
            [criterium.core :as criterium]
            [clojure.test :as t :refer [deftest]]
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [clojure.spec.test.alpha :as s.test]
            [undertaker.source.forgetful :as source.forgetful]))

(deftest benchmark-shrink-bytes
  (let [byte-arr (byte-array [0 0 0 0 1 2 3 4 5 6])]
    (criterium/quick-bench
      (undertaker/shrink-bytes byte-arr []))))

(deftest benchmark-byte-gen
  (let [source (source.forgetful/make-source (System/nanoTime))]
    (criterium/quick-bench
      (undertaker/byte source))))
