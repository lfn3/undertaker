(ns net.lfn3.undertaker.specs.source.forgetful
  (:require [net.lfn3.undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.source.forgetful :as source.forgetful]))



(s/fdef source.forgetful/make-source
        :args (s/cat :seed integer?)
        :ret (comp (partial extends? proto/ByteArraySource) class))
