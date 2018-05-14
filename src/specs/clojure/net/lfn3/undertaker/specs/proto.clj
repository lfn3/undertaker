(ns net.lfn3.undertaker.specs.proto
  (:require [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.specs.bytes]
            [net.lfn3.undertaker.proto :as proto]
            [clojure.test.check.generators :as gen]))

(s/def ::proto/interval-start nat-int?)
(s/def ::proto/interval-depth nat-int?)
(s/def ::proto/unique-hint (s/tuple (s/with-gen (partial = ::proto/unique)
                                                #(gen/return ::proto/unique)) int?))
(s/def ::proto/snippable-hint (s/tuple (s/with-gen (partial = ::proto/snippable)
                                                   #(gen/return ::proto/snippable)) nil?))
(s/def ::proto/hint (s/or :unique ::proto/unique-hint
                          :snippable ::proto/snippable-hint))
(s/def ::proto/hints (s/coll-of ::proto/hint))
(s/def ::proto/hints-for-next-interval ::proto/hints)

(s/def ::proto/wip-interval (s/keys :req [::proto/interval-start ::proto/hints ::proto/interval-depth]))

(s/def ::proto/interval-end nat-int?)
(s/def ::proto/generated-value (s/with-gen any? #(s/gen nil?)))
(s/def ::proto/mapped-bytes ::bytes/bytes)
(s/def ::proto/uniqueness-hint-id int?)

(s/def ::proto/interval (s/and (s/or :without-unique (s/keys :req [::proto/interval-start
                                                                   ::proto/hints
                                                                   ::proto/interval-depth
                                                                   ::proto/interval-end]
                                                             :opt [::proto/generated-value])
                                     :with-unique (s/keys :req [::proto/interval-start
                                                                ::proto/hints
                                                                ::proto/interval-depth
                                                                ::proto/interval-end
                                                                ::proto/mapped-bytes
                                                                ::proto/uniqueness-hint-id]
                                                          :opt [::proto/generated-value]))
                               (fn [[_ {:keys [::proto/interval-start
                                               ::proto/interval-end]}]]
                                 (<= interval-start interval-end))))

(s/def ::proto/interval-stack (s/coll-of (s/nilable ::proto/wip-interval)))
(s/def ::proto/completed-intervals (s/coll-of ::proto/interval))

(s/def ::proto/sampling? boolean?)
(s/def ::proto/debug? boolean?)
(s/def ::proto/shrinking? boolean?)
(s/def ::proto/bytes-requested nat-int?)
(s/def ::proto/source-used? boolean?)

(s/def ::proto/source-state (s/keys :req [::proto/sampling?
                                          ::proto/debug?
                                          ::proto/shrinking?
                                          ::proto/bytes-requested
                                          ::proto/hints-for-next-interval
                                          ::proto/interval-stack
                                          ::proto/completed-intervals
                                          ::bytes/byte-buffers]))