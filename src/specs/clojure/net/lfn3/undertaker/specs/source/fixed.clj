(ns net.lfn3.undertaker.specs.source.fixed
  (:require [net.lfn3.undertaker.source.fixed :as source.fixed]
            [net.lfn3.undertaker.bytes :as bytes]
            [clojure.spec.alpha :as s])
  (:import (net.lfn3.undertaker.source.fixed FixedSource)))


(s/fdef source.fixed/make-fixed-source
        :args (s/cat :bytes ::bytes/bytes)
        :ret (partial instance? FixedSource))
