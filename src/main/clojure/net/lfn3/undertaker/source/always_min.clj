(ns net.lfn3.undertaker.source.always-min
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.intervals :as intervals]
            [net.lfn3.undertaker.source.common :as source.common])
  (:import (java.nio ByteBuffer)
           (net.lfn3.undertaker ChainedByteBuffer)))

(defn initial-state []
  {::proto/interval-stack      []
   ::proto/completed-intervals []
   ::bytes/chained-byte-buffer (ChainedByteBuffer.)})

(defrecord AlwaysMinSource [state-atom]
  proto/ByteArraySource
  (get-bytes [_ ranges skip]
    (let [{:keys [::proto/interval-stack ::proto/completed-intervals]} @state-atom
          [ranges skip] (intervals/apply-hints interval-stack completed-intervals ranges skip)
          ranges (bytes/punch-skip-values-out-of-ranges skip ranges)
          flattened-ranges (mapcat identity ranges)]
      (if (every? nil? (map seq flattened-ranges))          ;i.e. range of size zero
        (byte-array 0)
        (let [min-range (loop [idx 0
                               ranges flattened-ranges]
                          (let [min-value (->> ranges
                                               (map #(nth %1 idx))
                                               (reduce min))
                                min-ranges (filter #(= min-value (nth %1 idx)) ranges)]
                            (cond
                              (= 0 (count min-ranges)) (first ranges)
                              (= 1 (count min-ranges)) (first min-ranges)
                              (< (inc idx) (count (last ranges))) (first min-ranges)
                              :default (recur (inc idx) min-ranges))))
              buf (ByteBuffer/wrap (byte-array min-range))]
          (.add (source.common/get-buffer state-atom) buf)
          buf))))
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
    (reset! state-atom (initial-state))))

(defn make-always-min-source []
  (let [state (atom (initial-state))]
    (->AlwaysMinSource state)))
