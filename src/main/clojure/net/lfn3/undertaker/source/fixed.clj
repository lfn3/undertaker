(ns net.lfn3.undertaker.source.fixed
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.intervals :as intervals])
  (:import (net.lfn3.undertaker OverrunException)
           (java.nio ByteBuffer)))

(defn initial-state [bytes]
  {::cursor                    0
   ::bytes/bytes               bytes
   ::proto/interval-stack      []
   ::proto/completed-intervals []
   ::bytes/byte-buffers []})

(defn reset-state [state]
  (-> state
      (assoc ::cursor 0)
      (assoc ::proto/completed-intervals [])
      (assoc ::proto/interval-stack [])
      (assoc ::bytes/byte-buffers [])))

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
      (swap! state-atom #(-> %1
                             (update ::cursor + size)
                             (update ::bytes/byte-buffers conj buf)))
      (bytes/map-into-ranges! buf ranges)
      buf))
  proto/Interval
  (push-interval [_ hints]
    (swap! state-atom intervals/push-interval hints)
    nil)
  (pop-interval [_ generated-value] (swap! state-atom intervals/pop-interval generated-value)
    nil)
  (get-intervals [_] (::proto/completed-intervals @state-atom))
  (get-wip-intervals [_] (::proto/interval-stack @state-atom))
  proto/Recall
  (get-sourced-byte-buffers [_] (::bytes/byte-buffers @state-atom))
  (reset [_] (swap! state-atom reset-state)))

(defn make-fixed-source [bytes]
  (let [state (atom (initial-state bytes))]
    (->FixedSource state)))
