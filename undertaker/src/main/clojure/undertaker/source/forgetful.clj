(ns undertaker.source.forgetful
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [undertaker.bytes :as bytes])
  (:import (java.util Random)))

(extend-type Random
  proto/ByteArraySource
  (get-bytes [this ranges skip-bytes]
    (let [unmapped (byte-array (count (first (first ranges))))]
      (.nextBytes this unmapped)
      (bytes/map-into-ranges unmapped ranges skip-bytes))))

(defrecord ForgetfulSource
  [rnd]
  proto/ByteArraySource
  (get-bytes [_ ranges skip] (proto/get-bytes rnd ranges skip))
  proto/Interval
  (push-interval [_ interval-name])
  (pop-interval [_ interval-id generated-value])
  (get-intervals [_] [])
  (get-wip-intervals [_] [])
  proto/Recall
  (get-sourced-bytes [_] (byte-array 0))
  (reset [_]))

(defn make-source [seed]
  (let [rnd (Random. seed)]
    (->ForgetfulSource rnd)))

(s/fdef make-source
  :args (s/cat :seed integer?)
  :ret (comp (partial extends? proto/ByteArraySource) class))
