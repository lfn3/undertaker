(ns undertaker.source.wrapped-random
  (:require [undertaker.proto :as proto]
            [undertaker.intervals :as intervals]
            [clojure.spec.alpha :as s]
            [undertaker.bytes :as bytes]
            [clojure.set :as set]
            [undertaker.debug :as debug])
  (:import (java.util Random)))

(defn get-bytes-from-java-random [^Random rnd ^long count]
  (let [arr (byte-array count)]
    (.nextBytes rnd arr)
    arr))

(defn- push-interval* [state interval-name hints]
  (let [id (inc (::proto/interval-id-counter state))]
    (-> state
        (update ::proto/interval-id-counter inc)
        (update ::proto/interval-stack conj {::proto/interval-name      interval-name
                                             ::proto/interval-id        id
                                             ::proto/interval-start     (count (get state ::bytes/bytes))
                                             ::proto/interval-parent-id (-> state
                                                                            ::proto/interval-stack
                                                                            (last)
                                                                            ::proto/interval-id)
                                             ::proto/hints              hints}))))

(defn- pop-interval* [state interval-id generated-value]
  (let [interval-to-update (last (::proto/interval-stack state))]
    (when (and debug/debug-mode
               (not= (::proto/interval-id interval-to-update) interval-id))
      (throw (debug/internal-exception "Popped interval without matching id"
                                       {:expected-id     interval-id
                                        :popped-interval interval-to-update
                                        :state           state})))
    (let [started-at (::proto/interval-start interval-to-update)
          ending-at (count (get state ::bytes/bytes))
          length (- ending-at started-at)]
      (-> state
          (update ::proto/interval-stack pop)
          (update ::proto/completed-intervals conj (-> interval-to-update
                                                 (assoc ::proto/interval-end ending-at)
                                                 (assoc ::proto/generated-value generated-value)
                                                 (assoc ::proto/mapped-bytes (->> state
                                                                                  ::bytes/bytes
                                                                                  (drop started-at)
                                                                                  (take length)
                                                                                  (vec)))))))))

(def initial-state {::proto/interval-id-counter  0
                    ::proto/interval-stack []
                    ::proto/completed-intervals  []
                    ::bytes/bytes                []})

(defrecord WrappedRandomSource
  [rnd state-atom]
  proto/ByteArraySource
  (get-bytes [this ranges skip]
    (let [bytes-to-generate (-> ranges (first) (first) (count))
          unmapped (get-bytes-from-java-random rnd bytes-to-generate)
          {:keys [::proto/interval-stack ::proto/completed-intervals]} @state-atom
          [ranges skip] (intervals/apply-hints interval-stack completed-intervals ranges skip)
          mapped (bytes/map-into-ranges unmapped ranges skip)]
      (swap! state-atom update ::bytes/bytes (fn [existing-bytes] (concat existing-bytes (vec mapped))))
      mapped))
  proto/Interval
  (push-interval [_ interval-name hints]
    (::proto/interval-id-counter (swap! state-atom push-interval* interval-name hints)))
  (pop-interval [_ interval-id generated-value]
    (swap! state-atom pop-interval* interval-id generated-value))
  (get-intervals [_] (::proto/completed-intervals @state-atom))
  (get-wip-intervals [_] (::proto/interval-stack @state-atom))
  proto/Recall
  (get-sourced-bytes [_]
    (-> state-atom
        deref
        ::bytes/bytes
        (byte-array)))
  (reset [_]
    (reset! state-atom initial-state)))

(defn make-source [seed]
  (let [rnd (Random. seed)
        state (if debug/debug-mode
                (atom initial-state :validator proto/source-state-validator)
                (atom initial-state))]
    (->WrappedRandomSource rnd state)))

(s/fdef make-source
  :args (s/cat :seed integer?)
  :ret (comp (partial extends? proto/ByteArraySource) class))
