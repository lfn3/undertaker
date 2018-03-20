(ns net.lfn3.undertaker.source
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.messages :as messages]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.intervals :as intervals]
            [net.lfn3.undertaker.source.fixed])
  (:import (net.lfn3.undertaker UniqueInputValuesExhaustedException UndertakerDebugException)
           (java.nio ByteBuffer)
           (net.lfn3.undertaker.source.fixed FixedSource)
           (java.util Collection)))

(def state-atom (atom {::sampling?                     true
                       ::debug?                        false
                       ::shrinking?                    false
                       ::source-in-use                 nil
                       ::bytes-requested               0
                       ::proto/hints-for-next-interval []
                       ::proto/interval-stack          []
                       ::proto/completed-intervals     []
                       ::bytes/byte-buffers            []}))

;; Most of this stuff is related to invariants & etc

(defn starting-test [source debug]
  (swap! state-atom assoc
         ::sampling? false
         ::debug? debug))
(defn starting-test-instance [source]
  (swap! state-atom assoc ::source-in-use source))
(defn completed-test-instance [source]
  (swap! state-atom assoc ::source-in-use nil))
(defn completed-test [source]
  (swap! state-atom assoc
         ::proto/interval-stack []
         ::proto/completed-intervals []
         ::bytes/byte-buffers []
         ::proto/hints-for-next-interval []
         ::bytes-requested 0
         ::sampling true
         ::debug false))

(defn shrinking? [] (::shrinking? @state-atom))
(defn shrinking! [] (swap! state-atom assoc ::shrinking? true))
(defn done-shrinking! [] (swap! state-atom assoc ::shrinking? false))

(defn internal-exception
  ([msg info] (internal-exception msg info nil))
  ([msg info cause]
   (UndertakerDebugException. (ex-info (str "DEBUGGING ERROR: " msg) info cause))))

(defn throw-if-source-is-nil [source]
  (when (nil? source)
    (throw (internal-exception (messages/missing-source-err-msg) {:source source
                                                                  :state @state-atom}))))

(defn every-call-in-scope-of-test-should-use-same-source [source]
  (when (not= source (::source-in-use @state-atom))
    (throw (internal-exception (messages/more-than-one-source-in-test-scope-err-msg) {:source source
                                                                                      :state @state-atom}))))

(defn should-only-use-fixed-source-while-shrinking [source]
  (when (and (shrinking?) (not (instance? FixedSource source)))
    (throw (internal-exception (messages/non-fixed-source-during-shrinking-error-msg) {:source source
                                                                                       :state @state-atom}))))

(defn check-invariants [source]
  (when (::debug? @state-atom)
    (throw-if-source-is-nil source)
    ;(every-call-in-scope-of-test-should-use-same-source source)
    (should-only-use-fixed-source-while-shrinking source)))

;; Used to source data for tests -  should usually be within the scope of `run-prop` unless we're sampling

(defn ^ByteBuffer get-bytes
  ([source ranges]
   (when (empty? ranges)
     (throw (IllegalArgumentException. "Ranges may not be empty.")))
    ;TODO: check we don't already have any ranges?
   (check-invariants source)
   (swap! state-atom update ::proto/interval-stack #(update %1 (dec (count %1)) assoc ::bytes/ranges ranges))
   (swap! state-atom update ::bytes-requested + (count (last (last ranges))))
   (let [{:keys [::proto/interval-stack ::proto/completed-intervals]} @state-atom
         hinted-ranges (intervals/apply-hints interval-stack completed-intervals ranges)]
     (when (empty? hinted-ranges)
       (throw (UniqueInputValuesExhaustedException. (str "Started with " (intervals/printable-ranges ranges)))))
     (let [buffer (proto/get-bytes source hinted-ranges)]
       (swap! state-atom update ::bytes/byte-buffers conj buffer)
       buffer))))

(defn add-hints-to-next-interval [source hints]
  (swap! state-atom update ::proto/hints-for-next-interval concat hints))

(defn push-interval
  ([source] (push-interval source []))
  ([source hints]
   (check-invariants source)
   (swap! state-atom intervals/push-interval hints)
   nil))

(defn pop-interval [source generated-value]
  (check-invariants source)
  (swap! state-atom #(cond-> %1
                       true (intervals/pop-interval generated-value)
                       (and (::sampling? %1)
                            (empty? (::proto/interval-stack %1))) (assoc ::proto/completed-intervals []
                                                                         ::bytes/byte-buffers []))))

; Used to gather data after a test run.

(defn get-wip-intervals [source] (::proto/interval-stack @state-atom))

(defn get-intervals [source] (::proto/completed-intervals @state-atom))

(defn reset [source]
  (swap! state-atom assoc
         ::proto/interval-stack      []
         ::proto/completed-intervals []
         ::bytes/byte-buffers        []
         ::bytes-requested           0)
  (proto/reset source))

(defn ^Collection get-sourced-byte-buffers [source]
  (::bytes/byte-buffers @state-atom))

(defn get-sourced-bytes [source]
  (bytes/buffers->bytes (::bytes/byte-buffers @state-atom)))

(defn add-source-data-to-results-map [source result-map]
  (let [success? (:net.lfn3.undertaker.core/result result-map)
        intervals (get-intervals source)]
    (cond-> result-map
      (::debug? @state-atom) (assoc ::proto/interval-stack (get-wip-intervals source))
      (::debug? @state-atom) (assoc ::proto/completed-intervals intervals)
      (::debug? @state-atom) (assoc :net.lfn3.undertaker.core/generated-bytes (vec (get-sourced-bytes source)))
      (::debug? @state-atom) (assoc :net.lfn3.undertaker.core/source source)
      true (assoc :net.lfn3.undertaker.core/source-used? (not (empty? intervals)))
      (not success?) (assoc :net.lfn3.undertaker.core/generated-values
                            (->> intervals
                                 (filter (comp zero? ::proto/interval-depth))
                                 (map ::proto/generated-value))))))
