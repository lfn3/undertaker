(ns undertaker.source.forgetful
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [undertaker.bytes :as bytes])
  (:import (java.util Random)
           (java.nio ByteBuffer)
           (com.lmax.undertaker ChainedByteBuffer)))

(defrecord ForgetfulSource
  [rnd]
  proto/ByteArraySource
  (get-bytes [_ ranges skip]
    (let [generated (byte-array (count (first (first ranges))))]
      (.nextBytes rnd generated)
      (bytes/map-into-ranges! (ByteBuffer/wrap generated) ranges skip)
      generated))
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

(s/fdef make-source
  :args (s/cat :seed integer?)
  :ret (comp (partial extends? proto/ByteArraySource) class))
