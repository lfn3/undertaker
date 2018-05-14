(ns net.lfn3.undertaker.source-test
  (:require [clojure.test :refer [deftest is] :as t]
            [net.lfn3.undertaker.source.forgetful :as source.forgetful]
            [net.lfn3.undertaker.source :as source]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as s.test]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.test-utils :as test-utils]))

(def forgetful-source (source.forgetful/make-source (System/nanoTime)))
(t/use-fixtures :each #(do (source/completed-test-instance forgetful-source)
                           (source/reset forgetful-source)
                           (source/starting-test-instance forgetful-source)
                           (%1)))

(test-utils/defchecks net.lfn3.undertaker.source)

(deftest should-emit-bytes
  (source/push-interval forgetful-source)
  (let [value (.get (source/get-bytes forgetful-source [[(byte-array [-128]) (byte-array [-1])]
                                                        [(byte-array [0]) (byte-array [127])]]))]
    (is bytes/byte? value)
    (is (contains? (set (range -128 127)) value))
    (source/pop-interval forgetful-source value)))

(deftest should-emit-positive-number
  (source/push-interval forgetful-source)
  (let [value (.get (source/get-bytes forgetful-source [[(byte-array [0]) (byte-array [127])]]))]
    (is pos-int? value)
    (is (contains? (set (range 0 127)) value))
    (source/pop-interval forgetful-source value)))

(deftest should-emit-unsigned-numbers-in-range
  (source/push-interval forgetful-source)
  (let [value (.get (source/get-bytes forgetful-source [[(byte-array [0]) (byte-array [0])]]))]
    (is (= 0 value))
    (source/pop-interval forgetful-source value))

  (source/push-interval forgetful-source)
  (let [values (byte-array 10)]
    (.get (source/get-bytes forgetful-source [[(byte-array (repeat 10 0)) (byte-array (repeat 10 1))]]) values)
    (is (not-every? (partial = 0) values))
    (is (not-every? (partial = 1) values))
    (source/pop-interval forgetful-source values))

  (source/push-interval forgetful-source)
  (let [size 10000
        values (byte-array size)]
    (.get (source/get-bytes forgetful-source [[(byte-array (repeat size -128)) (byte-array (repeat size -1))]
                                              [(byte-array (repeat size 0)) (byte-array (repeat size 127))]])
          values)
    (is (->> values
             (map (fn [val] [((set (range -128 128)) val) val]))
             (filter (comp nil? first))
             (empty?)))
    (source/pop-interval forgetful-source values)))
