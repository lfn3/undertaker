(ns net.lfn3.undertaker.source.fixed
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.source.state :as state])
  (:import (net.lfn3.undertaker OverrunException)
           (java.nio ByteBuffer)))

(defn initial-state [bytes]
  (merge (state/new-state)
         {::cursor      0
          ::bytes/bytes bytes
          ::proto/shrinking? true
          ::proto/sampling? false}))

(defrecord FixedSource [state-atom]
  proto/ByteArraySource
  (get-state-atom [_] state-atom)
  (get-bytes [_ state ranges]
    (let [size (count (first (first ranges)))
          bytes (byte-array (->> state
                                 (::bytes/bytes)
                                 (drop (::cursor state))
                                 (take size)))
          buf (ByteBuffer/wrap bytes)]
      (when-not (= (count bytes) size)
        (throw (OverrunException. (IndexOutOfBoundsException. (str "Tried to get " size " bytes from fixed source, "
                                                                   "but only " (count bytes) " were available.")))))
      [(update state ::cursor + size) buf]))
  (reset [_] (swap! state-atom assoc ::cursor 0)))

(defn make-fixed-source [bytes]
  (let [state (atom (initial-state bytes))]
    (->FixedSource state)))
