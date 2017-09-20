(ns undertaker.core-test
  (:require [clojure.test :as t :refer [deftest is]]
            [clojure.spec.test.alpha :as s.test]
            [orchestra.spec.test :as orchestra.test]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [undertaker.core :as undertaker]
            [clojure.test.check :as tcheck]
            [clojure.test.check.clojure-test :as tcheck-test]
            [clojure.test.check.properties :as tcheck-prop]
            [clojure.test.check.generators :as tcheck-gen]
            [undertaker.source :as source]
            [undertaker.source.fixed :as source.fixed]
            [undertaker.source.wrapped-random :as source.wrapped]
            [undertaker.proto :as proto]
            [undertaker.source.forgetful :as source.forgetful]
            [undertaker.source.always-max-source :as source.max]
            [undertaker.bytes :as bytes]))

(t/use-fixtures :once #(do (orchestra.test/instrument)
                           (%1)
                           (orchestra.test/unstrument)))

(def this-ns *ns*)

(def ignored #{`undertaker/from})

(deftest check-core
  (let [target-namespace (first (str/split (str this-ns) #"-test"))
        targets (->> (s/registry)
                     (filter #(str/starts-with? (str (key %1)) target-namespace))
                     (map first)
                     (remove ignored))
        result (s.test/check targets {:clojure.spec.test.check/opts {:num-tests 100}})
        failures (->> result
                      (filter #(-> %1
                                   (get-in [:clojure.spec.test.check/ret :result])
                                   (not)
                                   (true?))))]
    (println (str "Checked following specs in " target-namespace ": "))
    (dorun (map println targets))
    (is (empty? failures))))

(deftest test-boolean-gen
  (is (boolean? (undertaker/bool))))

(deftest test-int-gen
  (is (integer? (undertaker/int))))

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
  (let [results (repeatedly 10 #(undertaker/int -1 0))]
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
                 (<= val -1))
            (and (<= 3 val)
                 (<= val 5))))))

(deftest from-gen-test
  (let [values (set (repeatedly 3 undertaker/int))]
    (is (values (undertaker/from values)))))

(deftest long-gen-test
  (is (instance? Long (undertaker/long)))
  (is (>= (undertaker/long) Long/MIN_VALUE))
  (is (<= (undertaker/long) Long/MAX_VALUE)))

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
