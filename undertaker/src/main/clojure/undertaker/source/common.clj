(ns undertaker.source.common
  (:import (com.lmax.undertaker ChainedByteBuffer)))

(defn ^ChainedByteBuffer get-buffer [state-atom]
  (:undertaker.bytes/chained-byte-buffer @state-atom))
