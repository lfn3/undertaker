(ns net.lfn3.undertaker.source.sample
  (:require [net.lfn3.undertaker.source.forgetful :as source.forgetful]
            [net.lfn3.undertaker.proto :as proto]))

(defrecord SampleSource
  [wrapped-source]
  proto/ByteArraySource
  (get-bytes [_ ranges] (proto/get-bytes wrapped-source ranges))
  (reset [_] (proto/reset wrapped-source)))

(defn make-source [seed]
  (->SampleSource (source.forgetful/make-source seed)))
