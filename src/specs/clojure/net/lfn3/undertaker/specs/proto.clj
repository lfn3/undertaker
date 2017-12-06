(ns net.lfn3.undertaker.specs.proto
  (:require [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.specs.bytes]
            [net.lfn3.undertaker.proto :as proto]))

(s/def ::proto/interval-start nat-int?)
(s/def ::proto/interval-depth nat-int?)
(s/def ::proto/hint-applies-to #{::proto/immediate-children-of ::proto/this})
(s/def ::proto/hint-names #{::proto/unique ::proto/snippable})
(s/def ::proto/hint-args any?)
(s/def ::proto/hint (s/tuple ::proto/hint-applies-to ::proto/hint-names ::proto/hint-args))
(s/def ::proto/hints (s/coll-of ::proto/hint))

(s/def ::proto/wip-interval (s/keys :req [::proto/interval-start ::proto/hints ::proto/interval-depth]))

(s/def ::proto/interval-end nat-int?)
(s/def ::proto/generated-value (s/with-gen any? #(s/gen nil?)))
(s/def ::proto/mapped-bytes ::bytes/bytes)
(s/def ::proto/uniqueness-hint-id int?)

(s/def ::proto/interval (s/and (s/keys :req [::proto/interval-start
                                             ::proto/interval-end
                                             ::proto/hints]
                                       :opt [::proto/generated-value
                                             ::proto/mapped-bytes
                                             ::proto/uniqueness-hint-id])
                               (fn [{:keys [::proto/interval-start
                                            ::proto/interval-end]}]
                                 (<= interval-start interval-end))))

(s/def ::proto/interval-stack (s/coll-of ::proto/wip-interval))
(s/def ::proto/completed-intervals (s/coll-of ::proto/interval))

(s/def ::proto/source-state (s/keys :req [::proto/interval-stack
                                          ::proto/completed-intervals
                                          ::bytes/chained-byte-buffer]))

