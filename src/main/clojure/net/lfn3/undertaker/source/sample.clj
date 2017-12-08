(ns net.lfn3.undertaker.source.sample
  (:require [net.lfn3.undertaker.source.forgetful :as source.forgetful]
            [net.lfn3.undertaker.proto :as proto])
  (:import (java.util Random)))

(defrecord SampleSource
  [wrapped-source]
  proto/ByteArraySource
  (get-bytes [_ ranges] (proto/get-bytes wrapped-source ranges))
  proto/Interval
  (push-interval [_ hints] (proto/push-interval wrapped-source hints))
  (pop-interval [_ generated-value] (proto/pop-interval wrapped-source generated-value))
  (get-intervals [_] (proto/get-intervals wrapped-source))
  (get-wip-intervals [_] (proto/get-wip-intervals wrapped-source))
  proto/Recall
  (get-sourced-bytes [_] (proto/get-sourced-bytes wrapped-source))
  (reset [_] (proto/reset wrapped-source)))

(defn make-source [seed]
  (->SampleSource (source.forgetful/make-source seed)))
