(ns net.lfn3.undertaker.proto)

(defprotocol ByteArraySource
  (get-state-atom [this])
  (get-bytes [this state ranges])
  (reset [this]))
