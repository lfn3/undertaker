(ns undertaker.source.always-max
  (:require [undertaker.proto :as proto]
            [undertaker.intervals :as intervals]
            [undertaker.bytes :as bytes]
            [undertaker.source.common :as source.common])
  (:import (java.nio ByteBuffer)
           (com.lmax.undertaker ChainedByteBuffer)))

(defn initial-state []
  {::proto/interval-stack      []
   ::proto/completed-intervals []
   ::bytes/chained-byte-buffer (ChainedByteBuffer.)})

(defrecord AlwaysMaxSource [state-atom]
  proto/ByteArraySource
  (get-bytes [_ ranges skip]
    (let [{:keys [::proto/interval-stack ::proto/completed-intervals]} @state-atom
          [ranges skip] (intervals/apply-hints interval-stack completed-intervals ranges skip)
          ranges (bytes/punch-skip-values-out-of-ranges skip ranges)
          flattened-ranges (mapcat identity ranges)]
      (if (every? nil? (map seq flattened-ranges))          ;i.e. range of size zero
        (byte-array 0)
        (let [max-range (loop [idx 0
                               ranges flattened-ranges]
                          (let [max-value (->> ranges
                                               (map #(nth %1 idx))
                                               (reduce min))
                                max-ranges (filter #(= max-value (nth %1 idx)) ranges)]
                            (cond
                              (= 0 (count max-ranges)) (first ranges)
                              (= 1 (count max-ranges)) (first max-ranges)
                              (< (inc idx) (count (last ranges))) (first max-ranges)
                              :default (recur (inc idx) max-ranges))))
              generated (byte-array max-range)]
          (.add (source.common/get-buffer state-atom) (ByteBuffer/wrap generated))
          generated))))
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

(defn make-always-max-source []
  (let [state (atom (initial-state))]
    (->AlwaysMaxSource state)))
