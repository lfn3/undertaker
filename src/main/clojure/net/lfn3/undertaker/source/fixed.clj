(ns net.lfn3.undertaker.source.fixed
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes])
  (:import (net.lfn3.undertaker OverrunException)
           (java.nio ByteBuffer)))

(defn initial-state [bytes]
  {::cursor                    0
   ::bytes/bytes               bytes})

(defrecord FixedSource [state-atom]
  proto/ByteArraySource
  (get-bytes [_ ranges]
    (let [size (count (first (first ranges)))
          state @state-atom
          bytes (byte-array (->> state
                                 (::bytes/bytes)
                                 (drop (::cursor state))
                                 (take size)))
          buf (ByteBuffer/wrap bytes)]
      (when-not (= (count bytes) size)
        (throw (OverrunException. (IndexOutOfBoundsException. (str "Tried to get " size " bytes from fixed source, "
                                                                   "but only " (count bytes) " were available.")))))
      (bytes/map-into-ranges! buf ranges)
      buf))
  (reset [_] (swap! state-atom assoc ::cursor 0)))

(defn make-fixed-source [bytes]
  (let [state (atom (initial-state bytes))]
    (->FixedSource state)))
