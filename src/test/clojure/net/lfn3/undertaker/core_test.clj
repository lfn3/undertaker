(ns net.lfn3.undertaker.core-test
  (:require [clojure.test :as t :refer [deftest is]]
            [clojure.spec.test.alpha :as s.test]
            [orchestra.spec.test :as orchestra.test]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.core :as undertaker]
            [net.lfn3.undertaker.source.fixed :as source.fixed]
            [net.lfn3.undertaker.source.always-max :as source.max]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.specs.core]
            [net.lfn3.undertaker.test-utils :as test-utils])
  (:import (java.util Random)))

(test-utils/use-standard-fixtures)

(test-utils/defchecks net.lfn3.undertaker.core
                      #{net.lfn3.undertaker.core/elements
                        net.lfn3.undertaker.core/map-of
                        net.lfn3.undertaker.core/int
                        net.lfn3.undertaker.core/string
                        net.lfn3.undertaker.core/string-ascii
                        net.lfn3.undertaker.core/string-alphanumeric
                        net.lfn3.undertaker.core/string-alpha})

(deftest test-boolean-gen
  (is (boolean? (undertaker/boolean))))

(deftest test-int-gen
  (is (integer? (undertaker/int)))
  (is (= 0 (undertaker/int 0 0))))

(deftest test-vec-gen
  (is (vector? (undertaker/vec-of undertaker/int)))
  (is (every? int? (undertaker/vec-of undertaker/int)))
  (is (not-empty (undertaker/vec-of undertaker/byte 1))))

(deftest byte-gen-test
  (is (= 1 (undertaker/byte 1 1)))
  (let [results (repeatedly 10 #(undertaker/byte -1 0))]
    (is (not-every? (partial = 0) results))
    (is (not-every? (partial = -1) results))
    (is (every? #(or (= 0 %1) (= -1 %1)) results))))

(deftest should-generate-bytes-over-discontinuity
  (let [generated (repeatedly 1000 #(undertaker/byte -127 127))]
    (is (not-every? neg-int? generated))
    (is (not-every? pos-int? generated))))

(deftest byte-gen-should-emit-negative-values
  (is (neg-int? (undertaker/byte -128 -1))))

(deftest byte-gen-should-emit-positive-values
  (is (pos-int? (undertaker/byte 1 127))))

(deftest int-gen-test
  (is (= 1 (undertaker/int 1 1)))
  (let [results (repeatedly 100 #(undertaker/int -1 0))]
    (is (not-every? (partial = 0) results))
    (is (not-every? (partial = -1) results))
    (is (every? #(or (= 0 %1) (= -1 %1)) results))))

(deftest should-generate-ints-over-discontinuity
  (let [generated (repeatedly 1000 #(undertaker/int -10 10))]
    (is (every? (set generated) (set (range -10 11))))))

(deftest int-gen-should-emit--ve-values
  (let [val (undertaker/int Integer/MIN_VALUE -1)]
    (is (neg-int? val))))

(deftest can-generate-max-int
  (is (= Integer/MAX_VALUE (undertaker/int Integer/MAX_VALUE Integer/MAX_VALUE))))

(deftest can-generate-min-int
  (is (= Integer/MIN_VALUE (undertaker/int Integer/MIN_VALUE Integer/MIN_VALUE))))

(deftest int-gen-should-equals-signums-from--1-to-+1
  (let [val (undertaker/int -1 1)]
    (is (= (Integer/signum val) val))))

(deftest int-gen-should-generate-from-multiple-ranges
  (let [val (undertaker/int -5 -3 -1 1 3 5)]
    (is (or (and (<= -5 val)
                 (<= val -3))
            (and (<= -1 val)
                 (<= val 1))
            (and (<= 3 val)
                 (<= val 5))))))

(deftest from-gen-test
  (let [values (set (repeatedly 3 undertaker/int))]
    (is (values (undertaker/elements values)))))

(deftest long-gen-test                                      ;large-integer is aliased to long
  (is (instance? Long (undertaker/large-integer)))
  (is (>= (undertaker/large-integer) Long/MIN_VALUE))
  (is (<= (undertaker/large-integer) Long/MAX_VALUE)))

(deftest double-get-test
  (is (instance? Double (undertaker/double)))
  (is (>= (undertaker/double) (- Double/MAX_VALUE)))
  (is (<= (undertaker/double) Double/MAX_VALUE))
  (is (<= (undertaker/double -1.0 1.0) 1.0))
  (is (>= (undertaker/double -1.0 1.0) -1.0)))

(deftest double-without-NaN-test
  (with-bindings {#'undertaker/*source* (source.fixed/make-fixed-source [127 -8 0 0 0 0 0 0
                                                                         127 -8 0 0 0 0 0 0])}
    (is (not (Double/isNaN (undertaker/real-double)))))
  (with-bindings {#'undertaker/*source* (source.fixed/make-fixed-source [127 -15 0 0 0 0 0 0
                                                                         127 -15 0 0 0 0 0 0])}
    (is (not (Double/isNaN (undertaker/real-double)))))
  (with-bindings {#'undertaker/*source* (source.fixed/make-fixed-source [-1 -1 0 0 0 0 0 0
                                                                         -1 -1 0 0 0 0 0 0])}
    (is (not (Double/isNaN (undertaker/real-double)))))
  (with-bindings {#'undertaker/*source* (source.max/make-always-max-source)}
    (is (not (Double/isNaN (undertaker/real-double))))))

(deftest should-generate-max-double
  (with-bindings {#'undertaker/*source* (source.fixed/make-fixed-source (bytes/double->bytes Double/MAX_VALUE))}
    (let [value (undertaker/real-double)]
      (is (= Double/MAX_VALUE value) (str "Produced bytes were: " (vec (bytes/double->bytes value))
                                          ",\n       but expected: " (vec (bytes/double->bytes Double/MAX_VALUE)))))))

(deftest should-generate-min-double
  (with-bindings {#'undertaker/*source* (source.fixed/make-fixed-source (bytes/double->bytes Double/MIN_VALUE))}
    (let [value (undertaker/real-double)]
      (is (= Double/MIN_VALUE value) (str "Produced bytes were: " (vec (bytes/double->bytes value))
                                          ",\n       but expected: " (vec (bytes/double->bytes Double/MIN_VALUE)))))))

(deftest double-without-NaN-should-generate-numbers-in-range
  (with-bindings {#'undertaker/*source* (source.max/make-always-max-source)}
    (let [double (undertaker/real-double -1.0 1.0)]
      (is (<= double 1.0))
      (is (<= -1.0 double)))))

(deftest double-without-NaN-should-generate-numbers-above-one
  (with-bindings {#'undertaker/*source* (source.fixed/make-fixed-source [0 16 0 0 0 0 0 0])}
    (let [double (undertaker/real-double 1.0)]
      (is (<= 1.0 double)))))

(deftest short-should-generate-numbers-above-one
  (with-bindings {#'undertaker/*source* (source.fixed/make-fixed-source [0 0 0 0 0 0 0 0])}
    (let [short (undertaker/short 1)]
      (is (<= 1 short)))))

(deftest should-generate-set
  (with-bindings {#'undertaker/*source* (source.max/make-always-max-source)}
    (let [s (undertaker/set-of undertaker/int 5 5)]
      (is (= (count s) 5)))))

(deftest should-be-deterministic
  (let [seed (.nextInt (Random.))
        r1 (atom [])
        r2 (atom [])]
    (undertaker/run-prop {::undertaker/test-name "should-be-deterministic"
                          :seed seed}
      (fn [] (swap! r1 conj (undertaker/int))))
    (undertaker/run-prop {::undertaker/test-name "should-be-deterministic"
                          :seed seed}
      (fn [] (swap! r2 conj (undertaker/int))))
    (is (= @r1 @r2))))

(deftest frequency-works
  (let [count-of-vals (->> #(undertaker/frequency [[1 (constantly 1)]
                                                   [2 (constantly 2)]])
                           (repeatedly 1000)
                           (frequencies))
        number-of-twos (get count-of-vals 2)
        number-of-ones (get count-of-vals 1)
        ratio (/ number-of-twos (+ number-of-ones number-of-twos))]
    ;Should ~2/3
    (is (< 3/6 ratio))
    (is (< ratio 5/6))))

(deftest can-get-any
  (let [val (undertaker/any)]
    (is (any? val))))

(deftest can-get-any-printable                              ;TODO walk and check it's all ascii?
  (let [val (undertaker/any-printable)]
    (is (any? val))))

(deftest can-get-nat
  (let [n (undertaker/nat)]
    (is (not (zero? n)))
    (is (<= n 200))))

(deftest can-get-set-of-shorts
  (let [upper 1025
        lower 282
        source (source.fixed/make-fixed-source [1, 1, 27, 1, 4, 1, 0])]
    (with-bindings {#'undertaker/*source* source}
      (undertaker/set-of (partial undertaker/short lower upper)))))

(deftest can-gen-heterogeneous-set
  (let [s (undertaker/set-of #(undertaker/frequency [[1 undertaker/int]
                                                     [1 undertaker/double]])
                             10)]
    (is (= 10 (count s)))))

(deftest should-put-false-in-collections
  (let [v (undertaker/vec-of (constantly false) 1)]
    (is (= v [false]))))