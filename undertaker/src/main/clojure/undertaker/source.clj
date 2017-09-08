(ns undertaker.source
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [undertaker.source.wrapped-random]
            [undertaker.source.fixed]
            [undertaker.util :as util])
  (:import (java.util Random)))

(s/def ::source (s/with-gen (comp (partial extends? proto/ByteArraySource) class)
                            #(s.gen/fmap undertaker.source.wrapped-random/make-source (s.gen/int))))

(def source-in-use (atom #{}))
(defn done-with-test! [] (reset! source-in-use #{}))

(def shrinking (atom false))
(defn shrinking? [] @shrinking)
(defn shrinking! [] (reset! shrinking true))
(defn done-shrinking! [] (reset! shrinking false))

(defn missing-source-err-msg []
  "Source is nil.
This probably means you're missing part of the test setup.

If you're using Clojure:
  Switch this deftest for:
    (defprop test-name {}
      test-body...)

If you're using Java and jUnit:
  Add the junit test rule to the top of your file:
    @Rule
    public Source source = new SourceRule();")

(defn throw-if-source-is-nil [source]
  (when (nil? source)
    (throw (ex-info (missing-source-err-msg) {:source source}))))

(defn ^String more-than-one-source-in-test-scope-err-msg []
  (str "There's more than one source being used in this test.
This probably means you've manually supplied a source in a generator.
You should be using the automatically managed undertaker.core/*source*.

It might also mean you've found a bug, if so, please report it at " util/bug-tracker-url))

(defn every-call-in-scope-of-test-should-use-same-source [source]
  (when (and (not shrinking?) (not= 1 (count (swap! source-in-use conj source))))
    (throw (ex-info (more-than-one-source-in-test-scope-err-msg) {:source source}))))

(defn ^String non-fixed-source-during-shrinking-error-msg []
  (str "The source used during shrinking was not a fixed source.
This is most likely a bug in Undertaker, please report it at " util/bug-tracker-url))

(defn should-only-use-fixed-source-while-shrinking [source]
  (when (and (shrinking?) (not (instance? undertaker.source.fixed.FixedSource source)))
    (throw (ex-info (non-fixed-source-during-shrinking-error-msg) {:source source}))))

(defn check-invariants [source]
  (throw-if-source-is-nil source)
  (every-call-in-scope-of-test-should-use-same-source source)
  (should-only-use-fixed-source-while-shrinking source))

(defn get-bytes
  ([source ranges] (get-bytes source #{} ranges))
  ([source skip ranges]
   (check-invariants source)
   (proto/get-bytes source ranges skip)))

(defn push-interval [source interval-name]
  (check-invariants source)
  (proto/push-interval source interval-name))
(defn pop-interval [source interval-id generated-value]
  (check-invariants source)
  (proto/pop-interval source interval-id generated-value))
(defn get-intervals [source]
  (check-invariants source)
  (when-let [wip-intervals (seq (proto/get-wip-intervals source))]
    (throw (ex-info "Tried to get intervals when test has not finished generating input!"
                    {:source        source
                     :wip-intervals wip-intervals})))
  (proto/get-intervals source))

(defn get-sourced-bytes [source]
  (check-invariants source)
  (proto/get-sourced-bytes source))
(defn reset [source]
  (check-invariants source)
  (proto/reset source))

(defn used? [source]
  (check-invariants source)
  (not (empty? (proto/get-intervals source))))

