(ns undertaker.source.wrapped-random
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [undertaker.bytes :as bytes])
  (:import (java.util Random)))

(defn squish-ubyte [b ceiling]
  (let [range (inc (bit-and 0xff ceiling))]
    (cond
      (zero? range) 0
      (= 256 range) b
      :default (unchecked-byte (mod b range)))))

(extend-type Random
  proto/ByteArraySource
  (get-bytes [this ranges skip-bytes]
    (let [unmapped (byte-array (count (first (first ranges))))]
      (.nextBytes this unmapped)
      (bytes/map-into-ranges unmapped ranges skip-bytes))))

(s/def ::interval-id-counter int?)
(s/def ::bytes-counter int?)
(s/def ::completed-intervals (s/coll-of ::proto/interval))
(s/def ::frozen boolean?)
(s/def ::source-state (s/keys :req [::interval-id-counter
                                    ::bytes-counter
                                    ::proto/interval-stack
                                    ::completed-intervals
                                    ::frozen]))

(defn- push-interval* [state interval-name]
  (let [id (inc (::interval-id-counter state))]
    (-> state
        (update ::interval-id-counter inc)
        (update ::proto/interval-stack conj {::proto/interval-name      interval-name
                                             ::proto/interval-id        id
                                             ::proto/interval-start     (get state ::bytes-counter)
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
                                               (assoc ::proto/interval-end (get state ::bytes-counter))
                                               (assoc ::proto/generated-value generated-value))))))

(def initial-state {::interval-id-counter  0
                    ::bytes-counter        0
                    ::proto/interval-stack []
                    ::completed-intervals  []
                    ::frozen               false
                    ::bytes                []})

(defrecord WrappedRandomSource
  [rnd state-atom]
  proto/ByteArraySource
  (get-bytes [_ ranges skip]
    (let [bytes (proto/get-bytes rnd ranges skip)]
      (swap! state-atom #(-> %1
                             (update ::bytes-counter (partial + (count bytes)))
                             (update ::bytes (fn [existing-bytes] (concat existing-bytes (vec bytes))))))
      bytes))
  proto/Interval
  (push-interval [_ interval-name]
    (::interval-id-counter (swap! state-atom push-interval* interval-name)))
  (pop-interval [_ interval-id generated-value]
    (swap! state-atom pop-interval* interval-id generated-value))
  (get-intervals [_] (::completed-intervals @state-atom))
  (get-wip-intervals [_] (::proto/interval-stack @state-atom))
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
                                                            :bytes   (vec (::bytes state))})))
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
  :ret (comp (partial extends? proto/ByteArraySource) class))
