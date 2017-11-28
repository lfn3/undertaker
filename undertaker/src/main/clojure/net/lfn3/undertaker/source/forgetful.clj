(ns net.lfn3.undertaker.source.forgetful
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes])
  (:import (java.util Random)
           (java.nio ByteBuffer)
           (net.lfn3.undertaker ChainedByteBuffer)))

(defrecord ForgetfulSource
  [rnd]
  proto/ByteArraySource
  (get-bytes [_ ranges skip]
    (let [generated (byte-array (count (first (first ranges))))
          buf (ByteBuffer/wrap generated)]
      (.nextBytes rnd generated)
      (bytes/map-into-ranges! buf ranges skip)
      buf))
  proto/Interval
  (push-interval [_ hints])
  (pop-interval [_ generated-value])
  (get-intervals [_] [])
  (get-wip-intervals [_] [])
  proto/Recall
  (get-sourced-bytes [_] (-> (byte-array 0)
                             (ByteBuffer/wrap)
                             (ChainedByteBuffer.)))
  (reset [_]))

(defn make-source [seed]
  (let [rnd (Random. seed)]
    (->ForgetfulSource rnd)))