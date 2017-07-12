(ns undertaker.source.fixed
  (:require [undertaker.proto :as proto]))

(defn- push-interval* [state interval-name]
  (let [id (inc (::interval-id-counter state))]
    (-> state
        (update ::interval-id-counter inc)
        (update ::interval-stack conj [interval-name id (get state ::cursor)]))))

(defn- pop-interval* [state interval-id generated-value]
  (let [interval-to-update (last (::interval-stack state))]
    (when (not= (nth interval-to-update 1) interval-id)
      (throw (ex-info "Popped interval without matching id"
                      {:expected-id     interval-id
                       :popped-interval interval-to-update
                       :state           state})))
    (-> state
        (update ::interval-stack pop)
        (update ::completed-intervals conj (-> interval-to-update
                                               (conj (get state ::cursor))
                                               (conj generated-value))))))

(defn squish-byte [b floor ceiling]
  (-> b                                                     ;Most of the time the ranges should remain constant?
      (min ceiling)
      (max floor)))

;TODO should be pre-frozen - should be a validator checking bytes aren't modified
(defrecord FixedSource [state-atom]
  proto/ByteSource
  (get-byte [_ floor ceiling]
    (let [byte (nth (::bytes @state-atom) (::cursor @state-atom))]
      (swap! state-atom update ::cursor inc)
      (squish-byte byte floor ceiling)))
  proto/BytesSource
  (get-bytes [_ number mins maxes]
    (let [bytes (->> (::bytes @state-atom)
                     (drop (::cursor @state-atom))
                     (take number)
                     (map squish-byte mins maxes)
                     (byte-array))]
      (swap! state-atom update ::cursor + number)
      bytes))
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
    (swap! state-atom update ::cursor (constantly 0))))

(defn make-fixed-source [bytes intervals]
  (let [state (atom {::cursor              0
                     ::interval-id-counter 0
                     ::bytes               bytes
                     ::interval-stack      []
                     ::completed-intervals []
                     ::expected-intervals  intervals})]
    (->FixedSource state)))
