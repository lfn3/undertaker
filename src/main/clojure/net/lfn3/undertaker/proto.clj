(ns net.lfn3.undertaker.proto
  (:require [net.lfn3.undertaker.debug :as debug]))

(defprotocol ByteArraySource
  (get-bytes [this ranges]))

(defprotocol Interval
  (push-interval [this hints])
  (pop-interval [this generated-value])
  (get-intervals [this])
  (get-wip-intervals [this]))

(defprotocol Recall
  "Allows you to get the sequence of bytes this source of randomness has emitted since the last reset."
  (get-sourced-bytes [this])
  (reset [this]))
