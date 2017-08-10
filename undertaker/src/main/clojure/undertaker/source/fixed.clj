(ns undertaker.source.fixed
  (:require [undertaker.proto :as proto])
  (:import (com.lmax.undertaker OverrunException)))

(defn- push-interval* [state interval-name]
  (let [id (inc (::interval-id-counter state))]
    (-> state
        (update ::interval-id-counter inc)
        (update ::interval-stack conj {::proto/interval-name      interval-name
                                       ::proto/interval-id        id
                                       ::proto/interval-start     (get state ::cursor)
                                       ::proto/interval-parent-id (-> state
                                                                      ::interval-stack
                                                                      (last)
                                                                      ::proto/interval-id)}))))

(defn- pop-interval* [state interval-id generated-value]
  (let [interval-to-update (last (::interval-stack state))]
    (when (not= (::proto/interval-id interval-to-update) interval-id)
      (throw (ex-info "Popped interval without matching id"
                      {:expected-id     interval-id
                       :popped-interval interval-to-update
                       :state           state})))
    (-> state
        (update ::interval-stack pop)
        (update ::completed-intervals conj (-> interval-to-update
                                               (assoc ::proto/interval-end (get state ::cursor))
                                               (assoc ::proto/generated-value generated-value))))))

(defn squish-byte [b floor ceiling]
  (-> b                                                     ;Most of the time the ranges should remain constant?
      (min ceiling)
      (max floor)))

(defn squish-ubyte [b ceiling]
  (min (bit-and 0xff b) (bit-and 0xff ceiling)))

(defn byte-or-throw-overrun [state]
  (try (nth (::bytes state) (::cursor state))
       (catch IndexOutOfBoundsException e
         (throw (OverrunException. e)))))

(defn reset-state [state]
  (-> state
      (assoc ::cursor 0)
      (assoc ::completed-intervals [])
      (assoc ::interval-stack [])
      (assoc ::interval-id-counter 0)))

;TODO should be pre-frozen - should be a validator checking bytes aren't modified
(defrecord FixedSource [state-atom]
  proto/ByteSource
  (get-byte [_ floor ceiling]
    (let [byte (byte-or-throw-overrun @state-atom)]
      (swap! state-atom update ::cursor inc)
      (squish-byte byte floor ceiling)))
  proto/UnsignedByteSource
  (get-ubyte [_ ceiling]
    (let [byte (byte-or-throw-overrun @state-atom)]
      (swap! state-atom update ::cursor inc)
      (squish-ubyte byte ceiling)))
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
  (let [state (atom {::cursor              0
                     ::interval-id-counter 0
                     ::bytes               bytes
                     ::interval-stack      []
                     ::completed-intervals []})]
    (->FixedSource state)))
