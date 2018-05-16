(ns net.lfn3.undertaker.source.wrapped-random
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.source.state :as state]
            [net.lfn3.undertaker.intervals :as intervals])
  (:import (java.util Random)
           (java.nio ByteBuffer)))

(defn ^bytes fill-from-java-random [^Random rnd ^bytes arr]
  (.nextBytes rnd arr)
  arr)

(defn ^bytes get-bytes-from-java-random [^Random rnd ^long count]
  (let [arr (byte-array count)]
    (.nextBytes rnd arr)
    arr))

(defn initial-state [pre-genned]
  (merge (state/new-state)
         {::bytes/bytes          pre-genned
          ::remaining-pre-genned (count pre-genned)}))

(defn get-buffer [rnd state requested-size]
  (let [{:keys [::remaining-pre-genned ::bytes/bytes]} state
        within-existing (< requested-size remaining-pre-genned)
        byte-arr-size (count bytes)]
    (if within-existing
      (let [offset (- byte-arr-size remaining-pre-genned)
            buf (ByteBuffer/wrap bytes offset requested-size)]
        [(update state ::remaining-pre-genned - requested-size) buf])
      (let [new-arr-size (* 2 byte-arr-size)
            new-arr (get-bytes-from-java-random rnd new-arr-size)
            buf (ByteBuffer/wrap new-arr 0 requested-size)]
        [(assoc state ::remaining-pre-genned (- new-arr-size requested-size)
                      ::bytes/bytes new-arr)
         buf]))))

(defrecord WrappedRandomSource
  [rnd state-atom]
  proto/ByteArraySource
  (get-state-atom [_] state-atom)
  (get-bytes [_ state ranges]
    (let [number-of-bytes-requested (-> ranges (first) (first) (count))]
      (get-buffer rnd state number-of-bytes-requested)))
  (reset [_]
    (swap! state-atom #(assoc %1 ::remaining-pre-genned (count (::bytes/bytes %1))
                                 ::bytes/bytes (fill-from-java-random rnd (::bytes/bytes %1))))))

(defn make-source
  ([^long seed] (make-source seed #=(* 64 1024)))
  ([^long seed ^long size-to-pre-gen]
   (let [rnd (Random. seed)
         pre-genned (get-bytes-from-java-random rnd size-to-pre-gen)
         state (atom (initial-state pre-genned))]
     (->WrappedRandomSource rnd state))))
