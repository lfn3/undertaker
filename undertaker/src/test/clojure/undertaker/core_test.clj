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
            [undertaker.source.forgetful :as source.forgetful]))

(def forgetful-source (source.forgetful/make-source (System/nanoTime)))

(t/use-fixtures :once #(do (orchestra.test/instrument)
                           (with-bindings {#'undertaker/*source* forgetful-source}
                             (%1))
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

(deftest can-fail-prop
  (is (false? (::undertaker/result (undertaker/run-prop {} #(is false))))))

(deftest next-byte-for-int-should-emit-only-number-in-range
  (let [values (repeatedly 10 #(undertaker/generate-next-byte-for-int forgetful-source
                                                                      0
                                                                      true
                                                                      (byte-array [1])
                                                                      (byte-array [1])))]
    (is (every? (partial = 1) values)))
  (let [values (repeatedly 10 #(undertaker/generate-next-byte-for-int forgetful-source
                                                                      0
                                                                      true
                                                                      (byte-array [-1])
                                                                      (byte-array [-2])))]
    (is (every? #(or (= -2 %1)
                     (= -1 %1))
                values))))

(deftest from-gen-test
  (let [values (set (repeatedly 3 undertaker/int))]
    (is (values (undertaker/from values)))))

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
