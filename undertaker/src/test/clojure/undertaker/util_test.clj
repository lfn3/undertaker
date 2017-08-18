(ns undertaker.util-test
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

(t/use-fixtures :once #(do (orchestra.test/instrument)
                           (%1)
                           (orchestra.test/unstrument)))

(def this-ns *ns*)

(def ignored #{})

(deftest check-util-test
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

(deftest unsigned<=-test
  (is (util/unsigned<= 1 1))
  (is (util/unsigned<= 0 1))
  (is (util/unsigned<= 0 -1))
  (is (util/unsigned<= 0 -128))
  (is (util/unsigned<= 127 -128))
  (is (not (util/unsigned<= 1 0)))
  (is (not (util/unsigned<= -1 0)))
  (is (not (util/unsigned<= -128 0)))
  (is (not (util/unsigned<= -128 127))))

(deftest signed-range->unsigned-test
  (is (= 2 (util/signed-range->unsigned -1 1)))
  (is (= -1 (util/signed-range->unsigned -128 127)))
  (is (= 127 (util/signed-range->unsigned 0 127)))
  (is (= 32 (util/signed-range->unsigned 16 48)))
  (is (= -128 (util/signed-range->unsigned -1 127)))
  (is (= -128 (util/signed-range->unsigned -65 63))))

(deftest map-unsigned-byte-into-signed-range-test
  (is (= 0 (util/map-unsigned-byte-into-signed-range -128 127 0)))
  (is (= -128 (util/map-unsigned-byte-into-signed-range -128 127 -1))) ;-1 is the largest possible value
  (is (= 1 (util/map-unsigned-byte-into-signed-range -128 127 1)))
  (is (= 127 (util/map-unsigned-byte-into-signed-range -128 127 127)))
  (is (= -56 (util/map-unsigned-byte-into-signed-range -128 127 -73))))

(deftest map-unsigned-byte-into-unsigned-range-test
  (is (= 0 (util/map-unsigned-byte-into-unsigned-range -128 127 0)))
  (is (= -128 (util/map-unsigned-byte-into-unsigned-range -128 127 -1))) ;-1 is the largest possible value
  (is (= 1 (util/map-unsigned-byte-into-unsigned-range -128 127 1)))
  (is (= 127 (util/map-unsigned-byte-into-unsigned-range -128 127 127)))
  (is (= -56 (util/map-unsigned-byte-into-unsigned-range -128 127 -73)))
  (is (= 0 (util/map-unsigned-byte-into-unsigned-range -65 63 0)))
  (is (= 63 (util/map-unsigned-byte-into-unsigned-range -65 63 63)))
  (is (= -128 (util/map-unsigned-byte-into-unsigned-range -65 63 64)))
  (is (= -65 (util/map-unsigned-byte-into-unsigned-range -65 63 127))))
