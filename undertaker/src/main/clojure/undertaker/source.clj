(ns undertaker.source
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [undertaker.source.wrapped-random]
            [undertaker.util :as util])
  (:import (java.util Random)))

(s/def ::source (s/with-gen (comp (partial extends? proto/ByteSource) class)
                            #(s.gen/fmap undertaker.source.wrapped-random/make-source (s.gen/int))))

(defn missing-source-err-msg []
  "
  Source is nil.
  This probably means you're missing part of the test setup.

  If you're using Clojure:
    Either add the fixture to your test file:
      (clojure.test/use-fixtures :each undertaker.core/fixture)

    Or switch this deftest for:
      (defprop test-name {}
        test-body...)

  If you're using Java and jUnit:
    Add the junit test rule to the top of your file.
      @Rule
      public Source source = new SourceRule();")

(defn throw-if-source-is-nil [source]
  (when (nil? source)
    (throw (ex-info (missing-source-err-msg) {}))))

(defn get-byte
  ([source] (get-byte source Byte/MIN_VALUE Byte/MAX_VALUE))
  ([source min] (get-byte source min Byte/MAX_VALUE))
  ([source min max]
   (throw-if-source-is-nil source)
   (get-byte source min max)))

(s/fdef get-byte
  :args (s/cat :source ::source :min (s/? ::util/byte) :max (s/? ::util/byte))
  :ret (s/and ::util/byte
              (partial >= Byte/MAX_VALUE)
              (partial <= Byte/MIN_VALUE)))

(defn push-interval [source interval-name]
  (throw-if-source-is-nil source)
  (proto/push-interval source interval-name))
(defn pop-interval [source interval-id generated-value]
  (throw-if-source-is-nil source)
  (proto/pop-interval source interval-id generated-value))
(defn get-intervals [source]
  (throw-if-source-is-nil source)
  (proto/get-intervals source))

(defn get-sourced-bytes [source]
  (throw-if-source-is-nil source)
  (proto/get-sourced-bytes source))
(defn reset [source]
  (throw-if-source-is-nil source)
  (proto/reset source))

(defn used? [source]
  (throw-if-source-is-nil source)
  (not (empty? (proto/get-intervals source))))

