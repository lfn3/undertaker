(ns undertaker.proto)

(defprotocol ByteSource
  (get-byte [this]))

(defprotocol BytesSource
  (get-bytes [this number]))

(defprotocol Interval
  (push-interval [this interval-name])
  (pop-interval [this interval-id])
  (current-stack [this])
  (get-intervals [this]))

(defprotocol Recall
  "Allows you to get the sequence of bytes this source of randomness has emitted since the last reset using freeze.
   Freeze should also prevent the source of randomness from continuing to produce output until reset is called."
  (reset [this])
  (freeze [this]))
