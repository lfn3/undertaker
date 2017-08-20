(ns undertaker.source.always-zero-source
  (:require [undertaker.proto :as proto]))

(defn- push-interval* [state interval-name]
  (let [id (inc (::interval-id-counter state))]
    (-> state
        (update ::interval-id-counter inc)
        (update ::interval-stack conj {::proto/interval-name      interval-name
                                       ::proto/interval-id        id
                                       ::proto/interval-start     (count (get state ::bytes))
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
                                               (assoc ::proto/interval-end (count (get state ::bytes)))
                                               (assoc ::proto/generated-value generated-value))))))

(def initial-state {::interval-id-counter 0
                    ::bytes               []
                    ::interval-stack      []
                    ::completed-intervals []})

(defrecord AlwaysMaxSource [state-atom]
  proto/UnsignedByteSource
  (get-ubyte [_ ceiling]
    (swap! state-atom update ::bytes conj 0)
    0)
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
    (reset! state-atom initial-state)))

(defn make-always-zero-source []
  (let [state (atom initial-state)]
    (->AlwaysMaxSource state)))
