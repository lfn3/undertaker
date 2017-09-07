(ns undertaker.source.forgetful
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [undertaker.util :as util]
            [undertaker.bytes :as bytes])
  (:import (java.util Random)))

(defn squish-ubyte [b ceiling]
  (let [range (inc (bit-and 0xff ceiling))]
    (cond
      (zero? range) 0
      (= 256 range) b
      :default (unchecked-byte (mod b range)))))

(extend-type Random
  proto/UnsignedByteSource
  (get-ubyte [this max]
    (let [output (byte-array 1)]
      (.nextBytes this output)
      (squish-ubyte (aget output 0) max)))
  (get-bytes [this ranges skip-bytes]
    (let [unmapped (byte-array (count (first (first ranges))))]
      (.nextBytes this unmapped)
      (bytes/map-into-ranges unmapped ranges skip-bytes))))

(defrecord ForgetfulSource
  [rnd]
  proto/UnsignedByteSource
  (get-ubyte [_ max] (proto/get-ubyte rnd max))
  proto/ByteArraySource
  (get-bytes [_ ranges skip] (proto/get-bytes rnd ranges skip))
  proto/Interval
  (push-interval [_ interval-name])
  (pop-interval [_ interval-id generated-value])
  (get-intervals [_] [])
  (get-wip-intervals [_] [])
  proto/Recall
  (get-sourced-bytes [_] (byte-array 1))
  (reset [_]))

(defn make-source [seed]
  (let [rnd (Random. seed)]
    (->ForgetfulSource rnd)))

(s/fdef make-source
  :args (s/cat :seed integer?)
  :ret (comp (partial extends? proto/UnsignedByteSource) class))
