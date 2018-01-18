(ns net.lfn3.undertaker.proto)

(defprotocol ByteArraySource
  (get-bytes [this ranges])
  (reset [this]))
