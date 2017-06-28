(ns undertaker.source
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s])
  (:import (java.util Random)))

(defn get-bytes [source number]
  (if (extends? proto/BytesSource (class source))
    (proto/get-bytes source number)
    (byte-array (repeatedly number (proto/get-byte source)))))

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

(s/def ::interval-name string?)
(s/def ::interval-id int?)
(s/def ::interval-start int?)
(s/def ::interval-end int?)
(s/def ::wip-interval (s/tuple ::interval-name ::interval-id ::interval-start))
(s/def ::interval (s/tuple ::interval-name ::interval-id ::interval-start ::interval-end))

(s/def ::interval-id-counter int?)
(s/def ::bytes-counter int?)
(s/def ::interval-stack (s/coll-of ::wip-interval))
(s/def ::completed-intervals (s/coll-of ::interval))
(s/def ::frozen boolean?)
(s/def ::source-state (s/keys :req [::interval-id-counter
                                    ::bytes-counter
                                    ::interval-stack
                                    ::completed-intervals
                                    ::frozen]))

(defn push-interval* [state counter-key interval-name]
  (let [id (inc (::interval-id-counter state))]
    (-> state
        (update ::interval-id-counter inc)
        (update ::interval-stack conj [interval-name id (get state counter-key)]))))

(defn pop-interval* [state counter-key interval-id]
  (let [interval-to-update (last (::interval-stack state))]
    (when (not= (nth interval-to-update 1) interval-id)
      (throw (ex-info "Popped interval without matching id"
                      {:expected-id     interval-id
                       :popped-interval interval-to-update
                       :state           state})))
    (-> state
        (update ::interval-stack pop)
        (update ::completed-intervals conj (conj interval-to-update (get state counter-key))))))

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
    (::interval-id-counter (swap! state-atom push-interval* ::bytes-counter interval-name)))
  (pop-interval [_ interval-id]
    (swap! state-atom pop-interval* ::bytes-counter interval-id))
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
    (reset! state-atom initial-state)))

(defn make-source [seed]
  (let [rnd (Random. seed)
        state (atom initial-state)]
    (->WrappedRandomSource rnd state)))

;TODO should be pre-frozen - should be a validator checking bytes aren't modified
(defrecord FixedSource [state-atom]
  proto/ByteSource
  (get-byte [_]
    (let [byte (nth (::cursor @state-atom) (::bytes @state-atom))]
      (swap! state-atom update ::cursor inc)
      byte))
  proto/BytesSource
  (get-bytes [_ number]
    (let [bytes (-> (drop (::cursor @state-atom))
                    (take number))]
      (swap! state-atom update ::cursor + number)))
  proto/Interval
  (push-interval [_ interval-name]
    (::interval-id-counter (swap! state-atom push-interval* ::cursor interval-name)))
  (pop-interval [_ interval-id]
    (swap! state-atom pop-interval* ::cursor interval-id))
  (current-stack [_] (::interval-stack @state-atom))
  (get-intervals [_] (::completed-intervals @state-atom)))

(defn make-fixed-source [bytes intervals]
  (let [state (atom {::cursor              0
                     ::interval-id-counter 0
                     ::bytes               bytes
                     ::interval-stack      []
                     ::completed-intervals []
                     ::expected-intervals  intervals})]
    (->FixedSource state)))

