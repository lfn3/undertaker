(ns net.lfn3.undertaker.source
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.messages :as messages]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.debug :as debug]
            [net.lfn3.undertaker.intervals :as intervals]
            [net.lfn3.undertaker.source.fixed])
  (:import (net.lfn3.undertaker  UniqueInputValuesExhaustedException)
           (java.nio ByteBuffer)
           (net.lfn3.undertaker.source.fixed FixedSource)
           (java.util Collection)))

(def state-atom (atom {::sampling? true
                       ::shrinking? false
                       ::source-in-use nil
                       ::proto/interval-stack      []
                       ::proto/completed-intervals []
                       ::bytes/byte-buffers        []}))

(defn starting-test [source] (swap! state-atom assoc ::sampling? false))
(defn starting-test-instance [source] (swap! state-atom assoc ::source-in-use source))
(defn completed-test-instance [source] (swap! state-atom assoc ::source-in-use nil))
(defn completed-test [source] (swap! state-atom assoc ::sampling true))

(defn shrinking? [] (::shrinking? @state-atom))
(defn shrinking! [] (swap! state-atom assoc ::shrinking? true))
(defn done-shrinking! [] (swap! state-atom assoc ::shrinking? false))

(defn throw-if-source-is-nil [source]
  (when (nil? source)
    (throw (debug/internal-exception (messages/missing-source-err-msg) {:source source}))))

(defn every-call-in-scope-of-test-should-use-same-source [source]
  (when (not= source (::source-in-use @state-atom))
    (throw (debug/internal-exception (messages/more-than-one-source-in-test-scope-err-msg) {:source source}))))

(defn should-only-use-fixed-source-while-shrinking [source]
  (when (and (shrinking?) (not (instance? FixedSource source)))
    (throw (debug/internal-exception (messages/non-fixed-source-during-shrinking-error-msg) {:source source}))))

(defn check-invariants [source]
  (when debug/debug-mode
    (throw-if-source-is-nil source)
    (every-call-in-scope-of-test-should-use-same-source source)
    (should-only-use-fixed-source-while-shrinking source)))

(defn get-wip-intervals [source] (::proto/interval-stack @state-atom))

(defn get-intervals [source]
  (check-invariants source)
  (when-let [wip-intervals (and debug/debug-mode (seq (get-wip-intervals source)))]
    (throw (debug/internal-exception "Tried to get intervals when test has not finished generating input!"
                                     {:source        source
                                      :wip-intervals wip-intervals})))
  (::proto/completed-intervals @state-atom))

(defn ^ByteBuffer get-bytes
  ([source ranges]
   (check-invariants source)
   (let [interval-stack (get-wip-intervals source)
         completed-intervals (get-intervals source)
         ranges (intervals/apply-hints interval-stack completed-intervals ranges)]
     (when (empty? ranges)
       (throw (UniqueInputValuesExhaustedException. "Ran out of valid values to generate.")))
     (let [buffer (proto/get-bytes source ranges)]
       (swap! state-atom update ::bytes/byte-buffers conj buffer)
       buffer))))

(defn push-interval
  ([source] (push-interval source []))
  ([source hints]
   (check-invariants source)
   (swap! state-atom intervals/push-interval hints)
    nil))

(defn reset [source]
  (check-invariants source)
  (swap! state-atom assoc
         ::proto/interval-stack      []
         ::proto/completed-intervals []
         ::bytes/byte-buffers        [])
  (proto/reset source))

(defn pop-interval [source generated-value]
  (check-invariants source)
  (swap! state-atom #(cond-> %1
                             true (intervals/pop-interval generated-value)
                             (and (::sampling? %1)
                                  (empty? (::proto/interval-stack %1))) (assoc ::proto/completed-intervals []
                                                                               ::bytes/byte-buffers []))))

(defn ^Collection get-sourced-byte-buffers [source]
  (check-invariants source)
  (::bytes/byte-buffers @state-atom))

(defn get-sourced-bytes [source]
  (check-invariants source)
  (bytes/buffers->bytes (::bytes/byte-buffers @state-atom)))

(defn add-source-data-to-results-map [source result-map]
  (let [success? (:net.lfn3.undertaker.core/result result-map)
        intervals (get-intervals source)]
    (cond-> result-map
      debug/debug-mode (assoc :net.lfn3.undertaker.core/intervals intervals)
      debug/debug-mode (assoc :net.lfn3.undertaker.core/generated-bytes (get-sourced-bytes source))
      debug/debug-mode (assoc :net.lfn3.undertaker.core/source source)
      true (assoc :net.lfn3.undertaker.core/source-used? (not (empty? intervals)))
      (not success?) (assoc :net.lfn3.undertaker.core/generated-values
                            (->> intervals
                                 (filter (comp zero? ::proto/interval-depth))
                                 (map ::proto/generated-value))))))
