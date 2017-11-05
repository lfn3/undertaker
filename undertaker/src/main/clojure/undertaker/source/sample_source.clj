(ns undertaker.source.sample-source
  (:require [undertaker.source.forgetful :as source.forgetful]
            [undertaker.proto :as proto]
            [clojure.spec.alpha :as s])
  (:import (java.util Random)))

(defrecord SampleSource
  [wrapped-source]
  proto/ByteArraySource
  (get-bytes [_ ranges skip] (proto/get-bytes wrapped-source ranges skip))
  proto/Interval
  (push-interval [_ interval-name hints] (proto/push-interval wrapped-source interval-name hints))
  (pop-interval [_ interval-id generated-value] (proto/pop-interval wrapped-source interval-id generated-value))
  (get-intervals [_] (proto/get-intervals wrapped-source))
  (get-wip-intervals [_] (proto/get-wip-intervals wrapped-source))
  proto/Recall
  (get-sourced-bytes [_] (proto/get-sourced-bytes wrapped-source))
  (reset [_] (proto/reset wrapped-source)))

(defn make-source [seed]
  (->SampleSource (source.forgetful/make-source seed)))

(s/fdef make-source
        :args (s/cat :seed integer?)
        :ret (comp (partial extends? proto/ByteArraySource) class))
