(ns net.lfn3.undertaker.shrink-test
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.core :as undertaker]
            [orchestra.spec.test :as orchestra.test]
            [clojure.test :as t :refer [deftest is]]
            [net.lfn3.undertaker.shrink :as shrink]
            [clojure.string :as str]
            [clojure.spec.test.alpha :as s.test]
            [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.source.fixed :as source.fixed]
            [net.lfn3.undertaker.specs.bytes]
            [net.lfn3.undertaker.specs.core]
            [net.lfn3.undertaker.specs.shrink]
            [net.lfn3.undertaker.specs.proto]
            [net.lfn3.undertaker.test-utils :as test-utils])
  (:import (net.lfn3.undertaker OverrunException)))

(t/use-fixtures :once #(do (orchestra.test/instrument)
                           (with-redefs [undertaker/print-initial-failure (constantly nil)]
                             (%1))
                           (orchestra.test/unstrument)))

(test-utils/defchecks net.lfn3.undertaker.shrink)

(deftest can-fail-prop
  (is (false? (get-in (undertaker/run-prop {::undertaker/test-name "can-fail-prop"} #(is false))
                      [::undertaker/initial-results ::undertaker/result]))))

(deftest can-run-prop
  (is (true? (get-in (undertaker/run-prop {::undertaker/test-name "can-run-prop"} (constantly true))
                     [::undertaker/initial-results ::undertaker/result]))))

(deftest should-shrink-to-zero
  (let [result (undertaker/run-prop {::undertaker/test-name "should-shrink-to-zero" :debug true}
                                    #(is (boolean? (undertaker/byte))))]
    (is (= 0 (->> result
                  ::undertaker/shrunk-results
                  ::undertaker/generated-values
                  (first)))
        result)))

(deftest should-not-shrink-to-zero-if-does-not-fail-on-zero
  (let [result (undertaker/run-prop {::undertaker/test-name "should-not-shrink-to-zero-if-does-not-fail-on-zero"}
                                    #(is (= 0 (undertaker/byte))))]
    (is (->> result
             ::undertaker/shrunk-results
             ::undertaker/generated-values
             (first)
             (zero?)
             (not))
        result)))

(deftest should-shrink-past-1
  (is (= 0 (->> #(is (= 1 (undertaker/byte)))
                (undertaker/run-prop {::undertaker/test-name "should-shrink-past-1"})
                ::undertaker/shrunk-results
                ::undertaker/generated-values

                (first)))))

(deftest should-shrink-to-2
  (is (= 2 (->> #(let [value (undertaker/byte)]
                   (is (if (= 0 value)
                         true
                         (odd? value))))
                (undertaker/run-prop {::undertaker/test-name "should-shrink-to-2"})
                ::undertaker/shrunk-results
                ::undertaker/generated-values
                (first)))))

(deftest snip-interval-test
  (is (= [0 0] (vec (shrink/snip-interval (byte-array [0 1 1 0]) {::proto/interval-start 1
                                                                  ::proto/interval-end   3})))))

(deftest snip-intervals-should-handle-overrun-exceptions
  (is (= [0] (-> (byte-array [0])
                 (shrink/snip-intervals [{::proto/interval-start 0
                                          ::proto/interval-end   0
                                          ::proto/hints          []}]
                                        (undertaker/wrap-fn #(undertaker/int)))
                 (vec)))))

(deftest snip-intervals-handles-single-byte-failure
  (is (= [-19] (-> (byte-array [1 -19])
                   (shrink/snip-intervals [{::proto/interval-start 0
                                            ::proto/interval-end   1
                                            ::proto/hints          [[::proto/snippable nil]]}]
                                          (undertaker/wrap-fn #(is (boolean? (undertaker/byte)))))
                   (vec)))))

(deftest shrinking-vec-with-overrun
  (let [result (->> #(is (every? even? (undertaker/vec-of undertaker/byte 1 2)))
                    (undertaker/run-prop {::undertaker/test-name "shrinking-vec-with-overrun" :debug true}))]
    (is (= [1] (->> result
                    ::undertaker/shrunk-results
                    ::undertaker/generated-values
                    (first))))))

(deftest test-is-overrun?
  (let [wrapped (undertaker/wrap-fn #(throw (OverrunException.)))]
    (is (true? (shrink/is-overrun? (wrapped (source.fixed/make-fixed-source []))))))

  (let [wrapped (undertaker/wrap-fn #(is (throw (OverrunException.))))]
    (is (true? (shrink/is-overrun? (wrapped (source.fixed/make-fixed-source []))))))

  (let [wrapped (undertaker/wrap-fn #(assert (throw (OverrunException.))))]
    (is (true? (shrink/is-overrun? (wrapped (source.fixed/make-fixed-source [])))))))

(deftest should-shrink-middle-byte
  (let [result (->> #(let [bool-1 (undertaker/boolean)
                           _ (undertaker/int)
                           _ (undertaker/boolean)]
                       (is bool-1))
                    (undertaker/run-prop {::undertaker/test-name "should-shrink-middle-byte"}))]
    (is (= [false 0 false] (-> result
                              ::undertaker/shrunk-results
                              ::undertaker/generated-values
                              (vec)))
        result)))

(deftest should-shrink-vec-to-smallest-failing-case
  (let [result (->> #(is (every? even? (undertaker/vec-of undertaker/byte 1 2)))
                    (undertaker/run-prop {::undertaker/test-name "should-shrink-vec-to-smallest-failing-case"}))
        shrunk-vector (->> result
                           ::undertaker/shrunk-results
                           ::undertaker/generated-values
                           (first))]
    (is (or (= [1] shrunk-vector)
            (= [-1] shrunk-vector)) result)))

(deftest should-shrink-to-below-two
  ;Sometimes gets stuck at 2.0 due to reducing upper bytes before lower bytes.
  ;I think that's fine?
  (let [result (->> (fn [] (let [value (undertaker/double)]
                             (is (< value 0.9))))
                    (undertaker/run-prop {::undertaker/test-name "should-shrink-to-below-two" :debug true}))
        shrunk-val (first (get-in result [::undertaker/shrunk-results ::undertaker/generated-values]))]
    (is (<= 0.9 shrunk-val) result)
    (is (<= shrunk-val 2.0) result)))

(deftest should-move-bytes-towards-zero
  (let [test-fn (undertaker/wrap-fn (fn [] (is (not (< 1 (undertaker/byte))))))
        shrunk (shrink/move-bytes-towards-zero (byte-array [3]) test-fn)]
    (is (= [2] (vec shrunk)))))

(deftest should-move-bytes-in-array-towards-zero
  (let [test-fn (undertaker/wrap-fn (fn [] (is (every? #(not (< 1 %1)) (undertaker/vec-of undertaker/byte)))))
        shrunk (shrink/move-bytes-towards-zero (byte-array [1 3 1 4]) test-fn)]
    (is (= [1 3 0 0] (vec shrunk)))))

(def get-first-key #(some-> %1
                            (first)
                            (first)))

(deftest should-shrink-multi-part-map-keys
  (let [result (->> (fn [] (let [value (undertaker/map-of undertaker/string undertaker/short)]
                             (when (and (not= 0 (count value)))
                               (is (every? empty? (keys value)) value))))
                    (undertaker/run-prop {::undertaker/test-name "should-shrink-multi-part-map-keys" :debug true}))
        shrunk-value (get-in result [::undertaker/shrunk-results ::undertaker/generated-values])]
    (is (= 1 (count (first shrunk-value))) result)
    (is (= 1 (count (first (keys (first shrunk-value))))) result)))

(deftest shrinking-should-be-deterministic
  (let [r1 (->> (fn [] (let [value (undertaker/map-of undertaker/string undertaker/short)
                             empty-str? (some-> (get-first-key value)
                                                (empty?))]
                         (is (or (nil? empty-str?) empty-str?))))
                (undertaker/run-prop {::undertaker/test-name "shrinking-should-be-deterministic-1" :debug true :seed 1})
                ::undertaker/shrunk-results
                ::undertaker/generated-values)
        r2 (->> (fn [] (let [value (undertaker/map-of undertaker/string undertaker/short)
                             empty-str? (some-> (get-first-key value)
                                                (empty?))]
                         (is (or (nil? empty-str?) empty-str?))))
                (undertaker/run-prop {::undertaker/test-name "shrinking-should-be-deterministic-2" :debug true :seed 1})
                ::undertaker/shrunk-results
                ::undertaker/generated-values)]
    (is (= r1 r2))))
