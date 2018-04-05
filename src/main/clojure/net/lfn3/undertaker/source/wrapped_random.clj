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

(defrecord WrappedRandomSource
  [rnd state-atom]
  proto/ByteArraySource
  (get-bytes [_ ranges]
    (let [number-of-bytes-requested (-> ranges (first) (first) (count))
          {:keys [::remaining-pre-genned ::bytes/bytes]} @state-atom
          can-use-pregen? (< number-of-bytes-requested remaining-pre-genned)
          buf (if can-use-pregen?
                (let [offset (- (count bytes) remaining-pre-genned)]
                  (ByteBuffer/wrap bytes offset number-of-bytes-requested))
                (ByteBuffer/wrap (get-bytes-from-java-random rnd number-of-bytes-requested)))]
      (bytes/map-into-ranges! buf ranges)
      (swap! state-atom #(cond-> %1
                           can-use-pregen? (update ::remaining-pre-genned - number-of-bytes-requested)
                           true (update ::bytes/byte-buffers conj buf)))
      buf))
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
