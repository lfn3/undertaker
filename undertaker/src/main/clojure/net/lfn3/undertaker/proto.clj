(ns net.lfn3.undertaker.proto
  (:require [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.debug :as debug]))

(defn source-state-validator [state]
  (let [byte-counter (count (:net.lfn3.undertaker.bytes/bytes state))]
    (when-not (s/valid? ::source-state state)
      (throw (debug/internal-exception "Did not match spec" {:explained (s/explain ::source-state state)})))

    (when-let [overrunning-intervals (->> state
                                          ::completed-intervals
                                          (filter #(< byte-counter (::interval-start %1)))
                                          (seq))]
      (throw (debug/internal-exception "Intervals overran generated bytes" {:intervals    overrunning-intervals
                                                                            :byte-counter byte-counter})))
    (when-let [overrunning-intervals (->> state
                                          ::completed-intervals
                                          (filter #(< byte-counter (::interval-end %1)))
                                          (seq))]
      (throw (debug/internal-exception "Intervals overran generated bytes" {:intervals    overrunning-intervals
                                                                            :byte-counter byte-counter})))
    true))

(defprotocol ByteArraySource
  (get-bytes [this ranges skip]))

(defprotocol Interval
  (push-interval [this hints])
  (pop-interval [this generated-value])
  (get-intervals [this])
  (get-wip-intervals [this]))

(defprotocol Recall
  "Allows you to get the sequence of bytes this source of randomness has emitted since the last reset."
  (get-sourced-bytes [this])
  (reset [this]))
