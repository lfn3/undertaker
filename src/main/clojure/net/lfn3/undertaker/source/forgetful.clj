(ns net.lfn3.undertaker.source.forgetful
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes])
  (:import (java.util Random)
           (java.nio ByteBuffer)))

(defrecord ForgetfulSource
  [rnd]
  proto/ByteArraySource
  (get-bytes [_ ranges]
    (let [generated (byte-array (count (first (first ranges))))
          buf (ByteBuffer/wrap generated)]
      (.nextBytes rnd generated)
      (bytes/map-into-ranges! buf ranges)
      buf))
  (reset [_]))

(defn make-source [seed]
  (let [rnd (Random. seed)]
    (->ForgetfulSource rnd)))
