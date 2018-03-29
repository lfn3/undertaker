(ns net.lfn3.undertaker.specs.source
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [net.lfn3.undertaker.source :as source]
            [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.source.wrapped-random :as source.random]
            [net.lfn3.undertaker.bytes :as bytes])
  (:import (java.nio ByteBuffer)))

(s/def ::source/source (s/with-gen (comp (partial extends? proto/ByteArraySource) class)
                            #(s.gen/fmap source.random/make-source (s.gen/int))))

(s/def ::source/sampling? boolean?)
(s/def ::source/debug? boolean?)
(s/def ::source/shrinking? boolean?)
(s/def ::source/source-in-use (s/nilable ::source/source))
(s/def ::source/bytes-requested nat-int?)

(s/def ::source/state (s/keys :req [::source/sampling?
                                    ::source/debug?
                                    ::source/shrinking?
                                    ::source/source-in-use
                                    ::source/bytes-requested
                                    ::proto/hints-for-next-interval
                                    ::proto/interval-stack
                                    ::proto/completed-intervals
                                    ::bytes/byte-buffers]))

(s/fdef source/add-range-to-last-interval-if-not-nil
  :args (s/cat :source-state ::source/state :ranges ::bytes/ranges)
  :rest ::source/state)

(s/fdef source/get-bytes
  :args (s/cat :source ::source/source :skip (s/? ::bytes/bytes-to-skip) :ranges ::bytes/ranges)
  :ret (partial instance? ByteBuffer))

(s/fdef source/push-interval
        :args (s/cat :source ::source/source :hints (s/? ::proto/hints))
        :ret nil?)

(s/fdef source/get-sourced-byte-buffers
  :args (s/cat :source ::source/source)
  :ret ::bytes/byte-buffers)

(s/fdef source/get-sourced-bytes
  :args (s/cat :source ::source/source)
  :ret ::bytes/bytes)
