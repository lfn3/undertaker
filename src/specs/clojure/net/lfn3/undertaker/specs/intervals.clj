(ns net.lfn3.undertaker.specs.intervals
  (:require [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.intervals :as intervals]
            [net.lfn3.undertaker.specs.proto]
            [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.specs.bytes]
            [net.lfn3.undertaker.bytes :as bytes]))

(s/fdef intervals/apply-hints
  :args (s/cat :wip-intervals ::proto/interval-stack
               :completed-intervals ::proto/completed-intervals
               :ranges ::bytes/ranges)
  :ret ::bytes/ranges)

(s/fdef intervals/get-already-generated-when-unique
        :args (s/cat :hint ::proto/hint :wip-intervals ::proto/interval-stack :completed-intervals ::proto/completed-intervals)
        :ret ::bytes/bytes-to-skip)

(s/fdef intervals/pop-interval
        :args (s/cat :state ::proto/source-state :generated-value ::proto/generated-value)
        :ret ::proto/source-state)

(s/fdef intervals/push-interval
        :args (s/cat :state ::proto/source-state :hints ::proto/hints)
        :ret ::proto/source-state)

(s/fdef intervals/build-completed-interval
  :args (s/cat :wip-interval ::proto/wip-interval
               :generated-value (s/nilable ::proto/generated-value)
               :byte-buffers ::bytes/byte-buffers
               :uniqueness-hint-id (s/nilable ::proto/uniqueness-hint-id)
               :bytes-requested int?)
  :ret ::proto/interval)
