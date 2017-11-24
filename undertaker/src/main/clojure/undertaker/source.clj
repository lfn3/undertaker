(ns undertaker.source
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [undertaker.source.wrapped-random]
            [undertaker.source.fixed]
            [undertaker.messages :as messages]
            [undertaker.bytes :as bytes]
            [undertaker.debug :as debug])
  (:import (java.util Random)
           (com.lmax.undertaker ChainedByteBuffer)))

(s/def ::source (s/with-gen (comp (partial extends? proto/ByteArraySource) class)
                            #(s.gen/fmap undertaker.source.wrapped-random/make-source (s.gen/int))))

(def source-in-use (atom #{}))
(defn done-with-test! [] (reset! source-in-use #{}))

(def shrinking (atom false))
(defn shrinking? [] @shrinking)
(defn shrinking! [] (reset! shrinking true))
(defn done-shrinking! [] (reset! shrinking false))

(defn throw-if-source-is-nil [source]
  (when (nil? source)
    (throw (debug/internal-exception (messages/missing-source-err-msg) {:source source}))))

(defn every-call-in-scope-of-test-should-use-same-source [source]
  (when (and (not shrinking?) (not= 1 (count (swap! source-in-use conj source))))
    (throw (debug/internal-exception (messages/more-than-one-source-in-test-scope-err-msg) {:source source}))))

(defn should-only-use-fixed-source-while-shrinking [source]
  (when (and (shrinking?) (not (instance? undertaker.source.fixed.FixedSource source)))
    (throw (debug/internal-exception (messages/non-fixed-source-during-shrinking-error-msg) {:source source}))))

(defn check-invariants [source]
  (when debug/debug-mode
    (throw-if-source-is-nil source)
    (every-call-in-scope-of-test-should-use-same-source source)
    (should-only-use-fixed-source-while-shrinking source)))

(defn ^"[B" get-bytes
  ([source ranges] (get-bytes source #{} ranges))
  ([source skip ranges]
   (check-invariants source)
   (proto/get-bytes source ranges skip)))

(s/fdef get-bytes
  :args (s/cat :source ::source :skip (s/? ::bytes/bytes-to-skip) :ranges ::bytes/ranges)
  :ret bytes?)

(defn push-interval
  ([source] (push-interval source []))
  ([source hints]
   (check-invariants source)
   (proto/push-interval source hints)))

(s/fdef push-interval
  :args (s/cat :source ::source :hints (s/? ::proto/hints))
  :ret nil?)

(defn pop-interval [source generated-value]
  (check-invariants source)
  (proto/pop-interval source generated-value))
(defn get-intervals [source]
  (check-invariants source)
  (when-let [wip-intervals (and debug/debug-mode (seq (proto/get-wip-intervals source)))]
    (throw (debug/internal-exception "Tried to get intervals when test has not finished generating input!"
                                     {:source        source
                                      :wip-intervals wip-intervals})))
  (proto/get-intervals source))

(defn ^ChainedByteBuffer get-sourced-bytes [source]
  (check-invariants source)
  (proto/get-sourced-bytes source))

(s/fdef get-sourced-bytes
  :args (s/cat :source ::source)
  :ret (partial instance? ChainedByteBuffer))

(defn reset [source]
  (check-invariants source)
  (proto/reset source))

(defn used? [source]
  (check-invariants source)
  (not (empty? (proto/get-intervals source))))

