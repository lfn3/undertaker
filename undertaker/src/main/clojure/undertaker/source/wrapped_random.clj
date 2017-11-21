(ns undertaker.source.wrapped-random
  (:require [undertaker.proto :as proto]
            [undertaker.intervals :as intervals]
            [clojure.spec.alpha :as s]
            [undertaker.bytes :as bytes]
            [clojure.set :as set]
            [undertaker.debug :as debug]
            [undertaker.source.common :as source.common])
  (:import (java.util Random)
           (java.nio ByteBuffer)
           (com.lmax.undertaker ChainedByteBuffer)))

(defn ^bytes get-bytes-from-java-random [^Random rnd ^long count]
  (let [arr (byte-array count)]
    (.nextBytes rnd arr)
    arr))

(defn initial-state []
  {::proto/interval-stack      []
   ::proto/completed-intervals []
   ::bytes/chained-byte-buffer (ChainedByteBuffer.)})

(defrecord WrappedRandomSource
  [rnd state-atom]
  proto/ByteArraySource
  (get-bytes [this ranges skip]
    (let [bytes-to-generate (-> ranges (first) (first) (count))
          byte-array (get-bytes-from-java-random rnd bytes-to-generate)
          generated (ByteBuffer/wrap byte-array)
          {:keys [::proto/interval-stack ::proto/completed-intervals]} @state-atom
          [ranges skip] (intervals/apply-hints interval-stack completed-intervals ranges skip)]
      (bytes/map-into-ranges! byte-array ranges skip)
      (.add (source.common/get-buffer state-atom) generated)
      byte-array))
  proto/Interval
  (push-interval [_ hints]
    (swap! state-atom intervals/push-interval hints)
    nil)
  (pop-interval [_ generated-value]
    (swap! state-atom intervals/pop-interval generated-value)
    nil)
  (get-intervals [_] (::proto/completed-intervals @state-atom))
  (get-wip-intervals [_] (::proto/interval-stack @state-atom))
  proto/Recall
  (get-sourced-bytes [_]
    (-> state-atom
        (source.common/get-buffer)
        (.array)))
  (reset [_]
    (reset! state-atom (initial-state))))

(defn make-source [seed]
  (let [rnd (Random. seed)
        state (if debug/debug-mode
                (atom (initial-state) :validator proto/source-state-validator)
                (atom (initial-state)))]
    (->WrappedRandomSource rnd state)))

(s/fdef make-source
  :args (s/cat :seed integer?)
  :ret (comp (partial extends? proto/ByteArraySource) class))
