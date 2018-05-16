(ns net.lfn3.undertaker.source
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.source.state :as state]
            [net.lfn3.undertaker.source.fixed]
            [net.lfn3.undertaker.intervals :as intervals])
  (:import (java.nio ByteBuffer)
           (java.util Collection)
           (net.lfn3.undertaker UniqueInputValuesExhaustedException)))

(defn get-state-atom [source] (proto/get-state-atom source))
(defn get-state [source] @(proto/get-state-atom source))

;; Most of this stuff is related to invariants & etc

(defn starting-test [source debug] (swap! (get-state-atom source) state/starting-test debug))
(defn starting-test-instance [source] (swap! (get-state-atom source) state/starting-test-instance))
(defn completed-test-instance [source] (swap! (get-state-atom source) state/completed-test-instance))
(defn completed-test [source] (swap! (get-state-atom source) state/completed-test))

(defn ^ByteBuffer get-bytes
  ([source ranges]
   (when (empty? ranges)
     (throw (IllegalArgumentException. "Ranges may not be empty")))
   (let [state-atom (get-state-atom source)]
     (loop [{:keys [::proto/interval-stack ::proto/completed-intervals] :as old-state} @state-atom]
       (let [hints (intervals/hints-that-apply interval-stack)
             hinted-ranges (intervals/apply-hints interval-stack completed-intervals ranges)
             _ (when (empty? hinted-ranges)
                 (throw (UniqueInputValuesExhaustedException. (str "Started with " (intervals/printable-ranges ranges)))))
             [new-state buf] (proto/get-bytes source old-state hinted-ranges)
             new-state (state/add-range-and-buffer-to-state new-state buf hinted-ranges hints)]
         (if (compare-and-set! state-atom old-state new-state)
           (bytes/map-into-ranges! buf hinted-ranges)
           (recur @state-atom)))))))


(defn add-hints-to-next-interval [source hints]
  (swap! (get-state-atom source) state/add-hints-for-next-interval hints))

(defn push-interval
  ([source] (push-interval source []))
  ([source hints]
   (swap! (get-state-atom source) state/push-interval hints)
   nil))

(defn pop-interval [source generated-value]
  (swap! (get-state-atom source) state/pop-interval generated-value))

; Used to gather data after a test run.

(defn get-intervals [source] (state/get-intervals (get-state source)))

(defn reset [source]
  (swap! (get-state-atom source) state/reset)
  (proto/reset source))

(defn ^Collection get-sourced-byte-buffers [source] (state/get-sourced-byte-buffers (get-state source)))

(defn get-sourced-bytes [source]
  (bytes/buffers->bytes (get-sourced-byte-buffers source)))

(defn used? [source] (state/used? (get-state source)))

(defn add-source-data-to-results-map [source result-map]
  (let [{:keys [::debug] :as state} (get-state source)]
    (cond-> (state/add-source-data-to-results-map state result-map)
      debug (assoc :net.lfn3.undertaker.core/source source))))
