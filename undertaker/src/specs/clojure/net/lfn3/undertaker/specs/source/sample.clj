(ns net.lfn3.undertaker.specs.source.sample
  (:require [net.lfn3.undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.source.sample :as source.sample]))


(s/fdef source.sample/make-source
        :args (s/cat :seed integer?)
        :ret (comp (partial extends? proto/ByteArraySource) class))

