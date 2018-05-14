(ns net.lfn3.undertaker.specs.intervals
  (:require [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.intervals :as intervals]
            [net.lfn3.undertaker.specs.proto]
            [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.specs.bytes]
            [net.lfn3.undertaker.bytes :as bytes]
            [clojure.test.check.generators :as gen]))

(s/fdef intervals/apply-hints
  :args (s/cat :wip-intervals ::proto/interval-stack
               :completed-intervals ::proto/completed-intervals
               :ranges ::bytes/ranges)
  :ret ::bytes/ranges)

(s/def ::unique-hint-with-id (s/tuple (s/with-gen (partial = ::proto/unique)
                                                  #(gen/return ::proto/unique))
                                      int?))

(s/fdef intervals/get-already-generated-when-unique
        :args (s/cat :hint ::unique-hint-with-id :completed-intervals ::proto/completed-intervals)
        :ret ::bytes/bytes-to-skip)

(s/fdef intervals/pop-interval
        :args (s/cat :state (s/and ::proto/source-state
                                   (fn [{:keys [::proto/interval-stack]}]
                                     (not-empty interval-stack)))
                     :generated-value ::proto/generated-value)
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
