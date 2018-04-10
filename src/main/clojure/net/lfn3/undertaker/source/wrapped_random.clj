(ns net.lfn3.undertaker.source.wrapped-random
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes])
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
  {::bytes/bytes               pre-genned
   ::remaining-pre-genned      (count pre-genned)})

(defn get-buffer [rnd state-atom requested-size]
  (let [{:keys [::remaining-pre-genned ::bytes/bytes]} @state-atom
        within-existing (< requested-size remaining-pre-genned)
        byte-arr-size (count bytes)]
    (if within-existing
      (let [offset (- byte-arr-size remaining-pre-genned)
            buf (ByteBuffer/wrap bytes offset requested-size)]
        (swap! state-atom #(-> %1
                               (update ::remaining-pre-genned - requested-size)
                               (update ::bytes/byte-buffers conj buf)))
        buf)
      (let [new-arr-size (* 2 byte-arr-size)
            new-arr (get-bytes-from-java-random rnd new-arr-size)
            buf (ByteBuffer/wrap new-arr 0 requested-size)]
        (swap! state-atom #(-> %1
                               (assoc ::remaining-pre-genned requested-size
                                      ::bytes/bytes new-arr)
                               (update ::bytes/byte-buffers buf)))
        buf))))

(defrecord WrappedRandomSource
  [rnd state-atom]
  proto/ByteArraySource
  (get-bytes [_ ranges]
    (let [number-of-bytes-requested (-> ranges (first) (first) (count))
          buf (get-buffer rnd state-atom number-of-bytes-requested)]
      (bytes/map-into-ranges! buf ranges)
      buf))
  (reset [_]
    (swap! state-atom #(->> %1
                            ::bytes/bytes
                            (fill-from-java-random rnd)
                            (initial-state)))))

(defn make-source
  ([^long seed] (make-source seed #=(* 64 1024)))
  ([^long seed ^long size-to-pre-gen]
   (let [rnd (Random. seed)
         pre-genned (get-bytes-from-java-random rnd size-to-pre-gen)
         state (atom (initial-state pre-genned))]
     (->WrappedRandomSource rnd state))))
