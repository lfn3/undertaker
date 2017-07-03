(ns undertaker.source.wrapped-random
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [undertaker.source :as source])
  (:import (java.util Random)))

(extend-type Random
  proto/ByteSource
  (get-byte [this]
    (let [output (byte-array 1)]
      (.nextBytes this output)
      (aget output 0)))
  proto/BytesSource
  (get-bytes [this number]
    (let [output (byte-array number)]
      (.nextBytes this output)
      (vec output))))

(s/def ::interval-id-counter int?)
(s/def ::bytes-counter int?)
(s/def ::interval-stack (s/coll-of ::source/wip-interval))
(s/def ::completed-intervals (s/coll-of ::source/interval))
(s/def ::frozen boolean?)
(s/def ::source-state (s/keys :req [::interval-id-counter
                                    ::bytes-counter
                                    ::interval-stack
                                    ::completed-intervals
                                    ::frozen]))

(defn- push-interval* [state interval-name]
  (let [id (inc (::interval-id-counter state))]
    (-> state
        (update ::interval-id-counter inc)
        (update ::interval-stack conj [interval-name id (get state ::bytes-counter)]))))

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
                                               (conj (get state ::bytes-counter))
                                               (conj generated-value))))))

(def initial-state {::interval-id-counter 0
                    ::bytes-counter       0
                    ::interval-stack      []
                    ::completed-intervals []
                    ::frozen              false
                    ::bytes               []})

(defrecord WrappedRandomSource
  [rnd state-atom]
  proto/ByteSource
  (get-byte [_]
    (let [byte (proto/get-byte rnd)]
      (swap! state-atom #(-> %1
                             (update ::bytes-counter inc)
                             (update ::bytes conj byte)))
      byte))
  proto/BytesSource
  (get-bytes [_ number]
    (let [bytes (proto/get-bytes rnd number)]
      (swap! state-atom #(-> %1
                             (update ::bytes-counter + number)
                             (update ::bytes concat bytes)))
      bytes))
  proto/Interval
  (push-interval [_ interval-name]
    (::interval-id-counter (swap! state-atom push-interval* interval-name)))
  (pop-interval [_ interval-id generated-value]
    (swap! state-atom pop-interval* interval-id generated-value))
  (current-stack [_] (::interval-stack @state-atom))
  (get-intervals [_] (::completed-intervals @state-atom))
  proto/Recall
  (freeze [_]
    (let [current-state @state-atom]
      (if (empty? (::interval-stack current-state))
        (do
          (let [frozen-state (swap! state-atom assoc ::frozen true)]
            (set-validator! state-atom (fn [intended-state]
                                         (if (not= frozen-state intended-state)
                                           (throw (ex-info "Source has been frozen" {:state @state-atom}))
                                           true))))
          (::bytes current-state))
        (throw (ex-info "Not all intervals have been popped, cannot freeze yet" {:state current-state})))))
  (reset [_]
    (set-validator! state-atom nil)
    (reset! state-atom initial-state))
  (get-sourced-bytes [_]
    (-> state-atom
        deref
        ::bytes
        (byte-array))))

(defn make-source [seed]
  (let [rnd (Random. seed)
        state (atom initial-state :validator (partial s/valid? ::source-state))]
    (->WrappedRandomSource rnd state)))

(s/fdef make-source
        :args (s/cat :seed integer?)
        :ret (comp (partial extends? proto/BytesSource) class))
