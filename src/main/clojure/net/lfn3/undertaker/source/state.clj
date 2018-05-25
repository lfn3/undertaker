(ns net.lfn3.undertaker.source.state
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.intervals :as intervals]
            [net.lfn3.undertaker.bytes :as bytes])
  (:import (net.lfn3.undertaker UniqueInputValuesExhaustedException)
           (java.nio ByteBuffer)))

(defn new-state []
  {::proto/sampling?               true
   ::proto/debug?                  false
   ::proto/shrinking?              false
   ::proto/bytes-requested         0
   ::proto/hints-for-next-interval []
   ::proto/interval-stack          []
   ::proto/completed-intervals     []
   ::bytes/byte-buffers            []})

(defn starting-test [state debug] (assoc state ::proto/sampling? false
                                               ::proto/debug? debug))
;We reset ::bytes-requested here so we retain the used? state after a test is complete.
(defn starting-test-instance [state] (assoc state ::proto/bytes-requested 0))
(defn completed-test-instance [state] state)
(defn completed-test [state] (assoc state ::proto/interval-stack []
                                          ::proto/completed-intervals []
                                          ::bytes/byte-buffers []
                                          ::proto/hints-for-next-interval []
                                          ::proto/bytes-requested 0
                                          ::proto/sampling true
                                          ::proto/debug false))

;; Used to source data for tests -  should usually be within the scope of `run-prop` unless we're sampling

(defn add-range-and-buffer-to-state [{:keys [::proto/interval-stack ::proto/sampling?] :as state} buffer ranges hints]
  (let [bytes-requested (count (last (last ranges)))]
    (cond-> state
      true (update ::proto/bytes-requested + bytes-requested)
      (or (not sampling?) (not-empty hints)) (update ::bytes/byte-buffers conj buffer)
      (not (nil? (last interval-stack))) (update ::proto/interval-stack #(update %1 (dec (count %1))
                                                                                 assoc ::bytes/ranges ranges)))))

(defn add-hints-for-next-interval [state hints]
  (update state ::proto/hints-for-next-interval concat hints))

(defn push-interval [state hints]
  (intervals/push-interval state hints))

(defn reset [state]
  (assoc state
    ::proto/interval-stack []
    ::proto/completed-intervals []
    ::bytes/byte-buffers []
    ::proto/bytes-requested 0
    ::proto/hints-for-next-interval []))

(defn pop-interval [state generated-value]
  (cond-> state
    true (intervals/pop-interval generated-value)
    (and (::proto/sampling? state)
         (empty? (::proto/interval-stack state))) (reset)))

;;Used for gathering data after a test run

(defn get-intervals [state] (::proto/completed-intervals state))

(defn used? [state] (not (zero? (::proto/bytes-requested state))))


(defn get-sourced-byte-buffers [state] (::bytes/byte-buffers state))

(defn add-source-data-to-results-map [state result-map]
  (let [success? (:net.lfn3.undertaker.core/result result-map)
        {:keys [::proto/completed-intervals ::proto/interval-stack ::bytes/byte-buffers ::debug?]} state
        already-has-source-used? (contains? result-map ::proto/source-used?)]
    (cond-> result-map
      debug? (assoc ::proto/interval-stack interval-stack)
      debug? (assoc ::proto/completed-intervals completed-intervals)
      debug? (assoc :net.lfn3.undertaker.core/generated-bytes (vec (bytes/buffers->bytes byte-buffers)))
      (not already-has-source-used?) (assoc ::proto/source-used? (used? state))
      (not success?) (assoc :net.lfn3.undertaker.core/generated-values
                            (->> completed-intervals
                                 (filter (comp zero? ::proto/interval-depth))
                                 (map ::proto/generated-value))))))