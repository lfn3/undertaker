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
               :ranges ::bytes/ranges
               :skip ::bytes/bytes-to-skip)
  :ret (s/tuple ::bytes/ranges ::bytes/bytes-to-skip))

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
               :ending-at ::proto/interval-end
               :generated-value (s/nilable ::proto/generated-value)
               :chained-buffer ::bytes/chained-byte-buffer
               :uniqueness-hint-id (s/nilable ::proto/uniqueness-hint-id))
  :ret ::proto/interval)
