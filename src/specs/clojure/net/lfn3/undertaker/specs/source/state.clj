(ns net.lfn3.undertaker.specs.source.state
  (:require [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.source.state :as state]))

(s/fdef state/add-range-and-buffer-to-state
  :args (s/cat :source-state ::proto/source-state :ranges ::bytes/ranges :buffer ::bytes/byte-buffer)
  :rest ::proto/source-state)