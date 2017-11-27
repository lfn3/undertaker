(ns net.lfn3.undertaker.source.common
  (:import (net.lfn3.undertaker ChainedByteBuffer)))

(defn ^ChainedByteBuffer get-buffer [state-atom]
  (:net.lfn3.undertaker.bytes/chained-byte-buffer @state-atom))
