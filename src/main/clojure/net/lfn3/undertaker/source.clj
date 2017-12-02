(ns net.lfn3.undertaker.source
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.source.wrapped-random :as source.random]
            [net.lfn3.undertaker.source.fixed :as source.fixed]
            [net.lfn3.undertaker.messages :as messages]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.debug :as debug])
  (:import (net.lfn3.undertaker ChainedByteBuffer)
           (java.nio ByteBuffer)
           (net.lfn3.undertaker.source.fixed FixedSource)))

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
  (when (and (shrinking?) (not (instance? FixedSource source)))
    (throw (debug/internal-exception (messages/non-fixed-source-during-shrinking-error-msg) {:source source}))))

(defn check-invariants [source]
  (when debug/debug-mode
    (throw-if-source-is-nil source)
    (every-call-in-scope-of-test-should-use-same-source source)
    (should-only-use-fixed-source-while-shrinking source)))

(defn ^ByteBuffer get-bytes
  ([source ranges] (get-bytes source #{} ranges))
  ([source skip ranges]
   (check-invariants source)
   (proto/get-bytes source ranges skip)))

(defn push-interval
  ([source] (push-interval source []))
  ([source hints]
   (check-invariants source)
   (proto/push-interval source hints)))

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

(defn reset [source]
  (check-invariants source)
  (proto/reset source))

(defn used? [source]
  (check-invariants source)
  (not (empty? (proto/get-intervals source))))

(defn add-source-data-to-results-map [source result-map]
  (let [success? (:net.lfn3.undertaker.core/result result-map)
        intervals (get-intervals source)]
    (cond-> result-map
      debug/debug-mode (assoc :net.lfn3.undertaker.core/intervals intervals)
      debug/debug-mode (assoc :net.lfn3.undertaker.core/generated-bytes (-> source
                                                                            (get-sourced-bytes)
                                                                            (.array)
                                                                            (vec)))
      debug/debug-mode (assoc :net.lfn3.undertaker.core/source source)
      true (assoc :net.lfn3.undertaker.core/source-used? (not (empty? intervals)))
      (not success?) (assoc :net.lfn3.undertaker.core/generated-values
                            (map ::proto/generated-value
                                 (->> source
                                      (get-intervals)
                                      (filter (comp zero? ::proto/interval-depth))))))))
