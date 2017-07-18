(ns undertaker.source.wrapped-random
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s])
  (:import (java.util Random)))

(defn squish-byte [b floor ceiling]
  (let [range (inc (- ceiling floor))]
    (prn ceiling floor)
    (prn range)
    (unchecked-byte (+ floor (mod b range)))))

(extend-type Random
  proto/ByteSource
  (get-byte [this min max]
    (let [output (byte-array 1)]
      (.nextBytes this output)
      (squish-byte (aget output 0) min max))))

(s/def ::interval-id-counter int?)
(s/def ::bytes-counter int?)
(s/def ::interval-stack (s/coll-of ::proto/wip-interval))
(s/def ::completed-intervals (s/coll-of ::proto/interval))
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
  (get-byte [_ min max]
    (let [byte (proto/get-byte rnd min max)]
      (swap! state-atom #(-> %1
                             (update ::bytes-counter inc)
                             (update ::bytes conj byte)))
      byte))
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
    (swap! state-atom update ::bytes empty)))

(defn make-source [seed]
  (let [rnd (Random. seed)
        state (atom initial-state :validator (partial s/valid? ::source-state))]
    (->WrappedRandomSource rnd state)))

(s/fdef make-source
  :args (s/cat :seed integer?)
  :ret (comp (partial extends? proto/ByteSource) class))
