(ns net.lfn3.undertaker.intervals-test
  (:require [clojure.test :refer :all :as t]
            [clojure.spec.test.alpha :as s.test]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [orchestra.spec.test :as orchestra.test]
            [net.lfn3.undertaker.specs.intervals]
            [net.lfn3.undertaker.test-utils :as test-utils]))


(test-utils/use-standard-fixtures)

(test-utils/defchecks net.lfn3.undertaker.intervals
                      #{net.lfn3.undertaker.intervals/apply-hints
                        net.lfn3.undertaker.intervals/build-completed-interval
                        net.lfn3.undertaker.intervals/pop-interval})