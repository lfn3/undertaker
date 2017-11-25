(ns undertaker.source.fixed
  (:require [undertaker.proto :as proto]
            [undertaker.bytes :as bytes]
            [undertaker.debug :as debug]
            [clojure.spec.alpha :as s])
  (:import (com.lmax.undertaker OverrunException ChainedByteBuffer)
           (java.nio ByteBuffer)))

(defn- push-interval* [state hints]
  (update state ::proto/interval-stack conj {::proto/interval-start (get state ::cursor)
                                             ::proto/interval-depth (count (::proto/interval-stack state))
                                             ::proto/hints          hints}))

(defn- pop-interval* [state generated-value]
  (let [interval-to-update (last (::proto/interval-stack state))]
    (let [started-at (get interval-to-update ::proto/interval-start)
          ending-at (get state ::cursor)
          length (- ending-at started-at)]
      (-> state
          (update ::proto/interval-stack pop)
          (update ::proto/completed-intervals conj (-> interval-to-update
                                                       (assoc ::proto/interval-end ending-at)
                                                       (assoc ::proto/generated-value generated-value)
                                                       (assoc ::proto/mapped-bytes (->> state
                                                                                        ::bytes/bytes
                                                                                        (drop ending-at)
                                                                                        (take length)
                                                                                        (vec)))))))))

(defn initial-state [bytes]
  {::cursor                    0
   ::bytes/bytes               bytes
   ::proto/interval-stack      []
   ::proto/completed-intervals []})

(defn reset-state [state]
  (-> state
      (assoc ::cursor 0)
      (assoc ::proto/completed-intervals [])
      (assoc ::proto/interval-stack [])))

(defrecord FixedSource [state-atom]
  proto/ByteArraySource
  (get-bytes [_ ranges skip]
    (let [size (count (first (first ranges)))
          state @state-atom
          bytes (byte-array (->> state
                                 (::bytes/bytes)
                                 (drop (::cursor state))
                                 (take size)))
          buf (ByteBuffer/wrap bytes)]
      (when-not (= (count bytes) size)
        (throw (OverrunException. (IndexOutOfBoundsException. (str "Tried to get " size " bytes from fixed source, "
                                                                   "but only " (count bytes) " were available.")))))
      (swap! state-atom update ::cursor + size)
      (bytes/map-into-ranges! buf ranges skip)
      buf))
  proto/Interval
  (push-interval [_ hints]
    (swap! state-atom push-interval* hints)
    nil)
  (pop-interval [_ generated-value]
    (swap! state-atom pop-interval* generated-value)
    nil)
  (get-intervals [_] (::proto/completed-intervals @state-atom))
  (get-wip-intervals [_] (::proto/interval-stack @state-atom))
  proto/Recall
  (get-sourced-bytes [_]
    (->> state-atom
         deref
         ::bytes/bytes
         (byte-array)
         (ByteBuffer/wrap)
         (vector)
         (into-array ByteBuffer)
         (ChainedByteBuffer.)))
  (reset [_]
    (swap! state-atom reset-state)))

(defn make-fixed-source [bytes]
  (let [state (atom (initial-state bytes))]
    (->FixedSource state)))

(s/fdef make-fixed-source
  :args (s/cat :bytes ::bytes/bytes)
  :ret (partial instance? FixedSource))
