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

;TODO should be pre-frozen - should be a validator checking bytes aren't modified
(defrecord FixedSource [state-atom]
  proto/ByteSource
  (get-byte [_]
    (let [byte (nth (::bytes @state-atom) (::cursor @state-atom))]
      (swap! state-atom update ::cursor inc)
      byte))
  proto/BytesSource
  (get-bytes [_ number]
    (let [bytes-vec (->> (::bytes @state-atom)
                     (drop (::cursor @state-atom))
                     (take number))]
      (swap! state-atom update ::cursor + number)
      (byte-array bytes-vec)))
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
        (byte-array))))

(defn make-fixed-source [bytes intervals]
  (let [state (atom {::cursor              0
                     ::interval-id-counter 0
                     ::bytes               bytes
                     ::interval-stack      []
                     ::completed-intervals []
                     ::expected-intervals  intervals})]
    (->FixedSource state)))
