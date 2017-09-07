(ns undertaker.source.fixed
  (:require [undertaker.proto :as proto]
            [undertaker.bytes :as bytes])
  (:import (com.lmax.undertaker OverrunException)))

(defn- push-interval* [state interval-name]
  (let [id (inc (::interval-id-counter state))]
    (-> state
        (update ::interval-id-counter inc)
        (update ::proto/interval-stack conj {::proto/interval-name      interval-name
                                             ::proto/interval-id        id
                                             ::proto/interval-start     (get state ::cursor)
                                             ::proto/interval-parent-id (-> state
                                                                            ::proto/interval-stack
                                                                            (last)
                                                                            ::proto/interval-id)}))))

(defn- pop-interval* [state interval-id generated-value]
  (let [interval-to-update (last (::proto/interval-stack state))]
    (when (not= (::proto/interval-id interval-to-update) interval-id)
      (throw (ex-info "Popped interval without matching id"
                      {:expected-id     interval-id
                       :popped-interval interval-to-update
                       :state           state})))
    (-> state
        (update ::proto/interval-stack pop)
        (update ::completed-intervals conj (-> interval-to-update
                                               (assoc ::proto/interval-end (get state ::cursor))
                                               (assoc ::proto/generated-value generated-value))))))

(defn squish-ubyte [b ceiling]
  (unchecked-byte (min (bit-and 0xff b) (bit-and 0xff ceiling))))

(defn byte-or-throw-overrun [state]
  (try (nth (::bytes state) (::cursor state))
       (catch IndexOutOfBoundsException e
         (throw (OverrunException. e)))))

(defn reset-state [state]
  (-> state
      (assoc ::cursor 0)
      (assoc ::completed-intervals [])
      (assoc ::proto/interval-stack [])
      (assoc ::interval-id-counter 0)))

(defrecord FixedSource [state-atom]
  proto/UnsignedByteSource
  (get-ubyte [_ ceiling]
    (let [byte (byte-or-throw-overrun @state-atom)]
      (swap! state-atom update ::cursor inc)
      (squish-ubyte byte ceiling)))
  proto/ByteArraySource
  (get-bytes [_ ranges skip]
    (let [size (count (first (first ranges)))
          state @state-atom
          bytes (byte-array (->> state
                                 (::bytes)
                                 (drop (::cursor state))
                                 (take size)))]
      (when-not (= (count bytes) size) (throw (OverrunException.)))
      (swap! state-atom update ::cursor + size)
      (bytes/map-into-ranges bytes ranges skip)))
  proto/Interval
  (push-interval [_ interval-name]
    (::interval-id-counter (swap! state-atom push-interval* interval-name)))
  (pop-interval [_ interval-id generated-value]
    (swap! state-atom pop-interval* interval-id generated-value))
  (get-intervals [_] (::completed-intervals @state-atom))
  proto/Recall
  (get-sourced-bytes [_]
    (-> state-atom
        deref
        ::bytes
        (byte-array)))
  (reset [_]
    (swap! state-atom reset-state)))

(defn make-fixed-source [bytes]
  (let [state (atom {::cursor               0
                     ::interval-id-counter  0
                     ::bytes                bytes
                     ::proto/interval-stack []
                     ::completed-intervals  []})]
    (->FixedSource state)))
