(ns undertaker.java-int-gen-test
  (:require [clojure.test :refer [is deftest] :as t]
            [clojure.test.check :as t.check]
            [clojure.test.check.properties :as t.check.prop]
            [clojure.test.check.generators :as t.check.gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [undertaker.core :as undertaker]
            [clojure.test.check.generators :as gen]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as s.test])
  (:import (undertaker.generators IntGen)))

(defn wrapped-java-move-into-range [i min max]
  (IntGen/moveIntoRange i min max))

(s/fdef wrapped-java-move-into-range
  :args (s/cat :i int? :min int? :max int?)
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (and (<= (:min args) ret)
             (>= (:max args) ret))))

(deftest check-wrapped-move-into-range
  (s.test/check `wrapped-java-move-into-range))

(defspec move-into-range-is-the-same-as-clojure-impl
  (t.check.prop/for-all [i t.check.gen/int
                         [floor ceiling] (t.check.gen/bind t.check.gen/int
                                                           (fn [floor]
                                                             (let [ceiling (t.check.gen/fmap
                                                                             #(mod (+ floor %1) Integer/MAX_VALUE)
                                                                             t.check.gen/int)]
                                                               (t.check.gen/tuple
                                                                 (t.check.gen/return floor)
                                                                 ceiling))))]
    (= (undertaker/move-into-range i floor ceiling)
       (IntGen/moveIntoRange i floor ceiling))))
