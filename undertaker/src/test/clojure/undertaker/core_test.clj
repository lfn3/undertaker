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
            [undertaker.util :as util]))

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

(deftest long-gen-test
  (is (instance? Long (undertaker/long)))
  (is (>= (undertaker/long) Long/MIN_VALUE))
  (is (<= (undertaker/long) Long/MAX_VALUE)))

(deftest double-get-test
  (is (instance? Double (undertaker/double)))
  (is (>= (undertaker/double) Double/MIN_VALUE))
  (is (<= (undertaker/double) Double/MAX_VALUE))
  (is (<= (undertaker/double -1.0 1.0) 1.0))
  (is (>= (undertaker/double -1.0 1.0) -1.0)))

(deftest double-arrays-examples
  (is (= (vec (util/get-bytes-from-double -2.563353952042129E75))
         [-49 -106 -85 58 73 -49 -24 -102]))                ;This is the problem with the generator. -65 < -49 < 63.
  (is (= (vec (util/get-bytes-from-double 1.0))             ;But if we consider them as unsigned, -49 > -65.
         [63 -16 0 0 0 0 0 0]))
  (is (= (vec (util/get-bytes-from-double -1.0))
         [-65 -16 0 0 0 0 0 0]))
  (is (= (vec (util/get-bytes-from-double 0.5))
         [63 -32 0 0 0 0 0 0]))
  (is (= (vec (util/get-bytes-from-double -0.5))
         [-65 -32 0 0 0 0 0 0]))
  (is (= (vec (util/get-bytes-from-double 0.2))
         [63 -55 -103 -103 -103 -103 -103 -102]))
  (is (= (vec (util/get-bytes-from-double 1.0000000000000002))
         [63 -16 0 0 0 0 0 1])))

(deftest unsigned-range-test
  (is (= (undertaker/unsigned-range 0 0) 0))
  (is (= (undertaker/unsigned-range 0 1) 1))
  (is (= (undertaker/unsigned-range -1 0) 255))
  (is (= (undertaker/unsigned-range -128 0) 128))
  (is (= (undertaker/unsigned-range -128 127) 1))
  (is (= (undertaker/unsigned-range 127 -1) 128))
  (is (= (undertaker/unsigned-range 0 127) 127))
  (is (= (undertaker/unsigned-range -1 1) 254))
  (is (= (undertaker/unsigned-range -128 -1) 127))
  (is (= (undertaker/unsigned-range -65 63) 128)))

(deftest unsigned-range->generator-floor-and-ceiling-test
  (is (= [-128 127] (undertaker/unsigned-range->get-byte-floor-and-ceiling 255)))
  (is (= [0 127] (undertaker/unsigned-range->get-byte-floor-and-ceiling 128)))
  (is (= [-1 127] (undertaker/unsigned-range->get-byte-floor-and-ceiling 129))))

(deftest map-into-unsigned-range
  (is (= (undertaker/map-into-unsigned-range 0 -128 127) 0))
  (is (= (undertaker/map-into-unsigned-range -128 -128 127) -128))
  (is (= (undertaker/map-into-unsigned-range 127 -1 0) -1))
  (is (= (undertaker/map-into-unsigned-range 126 -1 0) -2)))
