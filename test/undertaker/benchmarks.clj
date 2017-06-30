(ns undertaker.benchmarks
  (:require [undertaker.core :as undertaker]
            [criterium.core :as criterium]
            [clojure.test :as t :refer [deftest]]))

(deftest benchmark-shrink-bytes
         (criterium/quick-bench (undertaker/shrink-bytes [0 0 0 0 1 2 3 4 5 6] [])))
