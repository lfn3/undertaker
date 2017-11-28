(ns net.lfn3.undertaker.source.wrapped-random
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.intervals :as intervals]
            [net.lfn3.undertaker.bytes :as bytes]
            [clojure.set :as set]
            [net.lfn3.undertaker.debug :as debug]
            [net.lfn3.undertaker.source.common :as source.common])
  (:import (java.util Random)
           (java.nio ByteBuffer)
           (net.lfn3.undertaker ChainedByteBuffer)))

(defn ^bytes fill-from-java-random [^Random rnd ^bytes arr]
  (.nextBytes rnd arr)
  arr)

(defn ^bytes get-bytes-from-java-random [^Random rnd ^long count]
  (let [arr (byte-array count)]
    (.nextBytes rnd arr)
    arr))

(defn initial-state [pre-genned]
  {::proto/interval-stack      []
   ::proto/completed-intervals []
   ::bytes/chained-byte-buffer (ChainedByteBuffer.)
   ::bytes/bytes pre-genned
   ::remaining-pre-genned (count pre-genned)})

(defrecord WrappedRandomSource
  [rnd state-atom]
  proto/ByteArraySource
  (get-bytes [this ranges skip]
    (let [number-of-bytes-requested (-> ranges (first) (first) (count))
          {:keys [::proto/interval-stack ::proto/completed-intervals ::remaining-pre-genned ::bytes/bytes]} @state-atom
          [ranges skip] (intervals/apply-hints interval-stack completed-intervals ranges skip)
          buf (if (< number-of-bytes-requested remaining-pre-genned)
                (let [offset (- (count bytes) remaining-pre-genned)]
                  (swap! state-atom update ::remaining-pre-genned - number-of-bytes-requested)
                  (ByteBuffer/wrap bytes offset number-of-bytes-requested))
                (ByteBuffer/wrap (get-bytes-from-java-random rnd number-of-bytes-requested)))]
      (bytes/map-into-ranges! buf ranges skip)
      (.add (source.common/get-buffer state-atom) buf)
      buf))
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
    (source.common/get-buffer state-atom))
  (reset [_]
    (swap! state-atom #(->> %1
                            ::bytes/bytes
                            (fill-from-java-random rnd)
                            (initial-state)))))

(defn make-source
  ([^long seed] (make-source seed 0))
  ([^long seed ^long size-to-pre-gen]
   (let [rnd (Random. seed)
         pre-genned (get-bytes-from-java-random rnd size-to-pre-gen)
         state (atom (initial-state pre-genned))]
     (->WrappedRandomSource rnd state))))
