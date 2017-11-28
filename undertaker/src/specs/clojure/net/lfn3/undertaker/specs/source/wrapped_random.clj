(ns net.lfn3.undertaker.specs.source.wrapped-random
  (:require [net.lfn3.undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.source.wrapped-random :as source.random]))

(s/fdef source.random/make-source
        :args (s/cat :seed integer? :size-to-pre-gen (s/? integer?))
        :ret (comp (partial extends? proto/ByteArraySource) class))
