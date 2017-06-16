(ns undertaker.proto)

(defprotocol ByteSource
  (get-byte [this]))

(defprotocol BytesSource
  (get-bytes [this number]))
