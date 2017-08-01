(ns undertaker.shrinking-test
  (:require [undertaker.proto :as proto]
            [undertaker.core :as undertaker]
            [orchestra.spec.test :as orchestra.test]
            [clojure.test :as t :refer [deftest is]]))


(t/use-fixtures :once #(do (orchestra.test/instrument)
                           (%1)
                           (orchestra.test/unstrument)))

(deftest can-fail-prop
  (is (false? (::undertaker/result (undertaker/run-prop {} #(is false))))))

(deftest can-run-prop
  (is (true? (::undertaker/result (undertaker/run-prop {} (constantly true))))))

(deftest should-shrink-to-zero
  (is (= 0 (->> #(is (boolean? (undertaker/byte)))
                (undertaker/run-prop {})
                ::undertaker/shrunk-values
                (first)))))

(deftest should-not-shrink-to-zero-if-does-not-fail-on-zero-prop
  (is (->> #(is (= 0 (undertaker/byte)))
           (undertaker/run-prop {})
           ::undertaker/shrunk-values
           (first)
           (zero?)
           (not))))

(deftest should-shrink-two-steps
  (is (= [0] (vec (proto/get-sourced-bytes (undertaker/shrink (byte-array [2])
                                                              []
                                                              (constantly {::undertaker/result false})))))))

(deftest should-not-shrink-to-zero-if-does-not-fail-on-zero-shrinker
  (is (not (zero? (-> (undertaker/shrink (byte-array [2])
                                         []
                                         (undertaker/wrap-fn (fn [] {::undertaker/result (is (= 0 (undertaker/byte)))})))
                      (proto/get-sourced-bytes)
                      (first))))))

(deftest should-shrink-past-1
  (is (= [0] (-> (undertaker/shrink (byte-array [5])
                                    []
                                    (undertaker/wrap-fn (fn [] {::undertaker/result (is (= 1 (undertaker/byte)))})))
                 (proto/get-sourced-bytes)
                 (vec)))))

(deftest should-shrink-to-2
  (is (= [2] (-> (undertaker/shrink (byte-array [80])
                                    []
                                    (undertaker/wrap-fn (fn [] {::undertaker/result (let [value (undertaker/byte)]
                                                                                      (is (if (= 0 value)
                                                                                            true
                                                                                            (odd? value))))})))
                 (proto/get-sourced-bytes)
                 (vec)))))

(deftest snip-interval-test
  (is (= [0 0] (vec (undertaker/snip-interval (byte-array [0 1 1 0]) {::proto/interval-start 1
                                                                      ::proto/interval-end   3})))))

(deftest snip-intervals-should-handle-overrun-exceptions
  (is (= [0] (-> (byte-array [0])
                 (undertaker/snip-intervals [{::proto/interval-start 0
                                              ::proto/interval-end   0}]
                                            (undertaker/wrap-fn #(undertaker/int)))
                 (vec)))))

(deftest snip-intervals-handles-single-byte-failure
  (is (= [-19] (-> (byte-array [1 -19])
                   (undertaker/snip-intervals [{::proto/interval-start 0
                                                ::proto/interval-end   1}]
                                              (undertaker/wrap-fn #(is (boolean? (undertaker/byte)))))
                   (vec)))))

(deftest should-shrink-middle-byte
  (let [result (->> #(let [bool-1 (undertaker/bool)
                           a-number (undertaker/int)
                           bool-2 (undertaker/bool)]
                       (is (not bool-1)))
                    (undertaker/run-prop {}))]
    (is (= [true 0 false] (-> result
                              ::undertaker/shrunk-values
                              (vec)))
        result)))

(deftest should-shrink-vec-to-smallest-failing-case
  (let [result (->> #(let [values (undertaker/vec-of undertaker/byte)]
                       (is (every? even? values)))
                    (undertaker/run-prop {}))
        shrunk-vector (->> result
                           ::undertaker/shrunk-values
                           (first))]
    (is (or (= [1] shrunk-vector)
            (= [-1] shrunk-vector)) result)))

(deftest should-only-run-once-since-source-is-unused
  (let [result (->> (fn [])
                    (undertaker/run-prop {}))]
    (is (= 1 (::undertaker/iterations-run result)))))
