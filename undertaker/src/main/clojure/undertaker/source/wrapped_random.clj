(ns undertaker.source.wrapped-random
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [undertaker.util :as util])
  (:import (java.util Random)))

(defn squish-byte [b floor ceiling]
  (let [range (inc (util/abs (- ceiling floor)))]
    (unchecked-byte (+ floor (mod b range)))))

(defn squish-ubyte [b ceiling]
  (let [range (inc (bit-and 0xff ceiling))]
    (cond
      (zero? range) 0
      (= 256 range) b
      :default (unchecked-byte (mod b range)))))

(extend-type Random
  proto/ByteSource
  (get-byte [this min max]
    (let [output (byte-array 1)]
      (.nextBytes this output)
      (squish-byte (aget output 0) min max)))
  proto/UnsignedByteSource
  (get-ubyte [this max]
    (let [output (byte-array 1)]
      (.nextBytes this output)
      (squish-ubyte (aget output 0) max))))

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
        (update ::interval-stack conj {::proto/interval-name      interval-name
                                       ::proto/interval-id        id
                                       ::proto/interval-start     (get state ::bytes-counter)
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
                                               (assoc ::proto/interval-end (get state ::bytes-counter))
                                               (assoc ::proto/generated-value generated-value))))))

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
  proto/UnsignedByteSource
  (get-ubyte [_ max]
    (let [byte (proto/get-ubyte rnd max)]
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
    (reset! state-atom initial-state)))

(defn state-validator [state]
  (when-not (s/valid? ::source-state state)
    (throw (ex-info "Did not match spec" {:explained (s/explain ::source-state state)})))
  (when-not (= (::bytes-counter state) (count (::bytes state)))
    (throw (ex-info "Bytes counter out of step with array" {:counter (::bytes-counter state)
                                                            :bytes (vec (::bytes state))})))
  (->> state
       (map ::completed-intervals)
       (filter #(>= (::bytes-counter state) (::proto/interval-start %1))))
  (->> state
       (map ::completed-intervals)
       (filter #(>= (::bytes-counter state) (::proto/interval-end %1)))))

(defn make-source [seed]
  (let [rnd (Random. seed)
        state (atom initial-state :validator state-validator)]
    (->WrappedRandomSource rnd state)))

(s/fdef make-source
  :args (s/cat :seed integer?)
  :ret (comp (partial extends? proto/ByteSource) class))
