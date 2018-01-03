(ns net.lfn3.undertaker.source.always-min
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.intervals :as intervals])
  (:import (java.nio ByteBuffer)))

(defn initial-state []
  {::proto/interval-stack      []
   ::proto/completed-intervals []
   ::bytes/byte-buffers        []})

(defrecord AlwaysMinSource [state-atom]
  proto/ByteArraySource
  (get-bytes [_ ranges]
    (let [flattened-ranges (mapcat identity ranges)]
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
          (swap! state-atom update ::bytes/byte-buffers conj buf)
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
  (get-sourced-byte-buffers [_]
    (::bytes/byte-buffers @state-atom))
  (reset [_]
    (reset! state-atom (initial-state))))

(defn make-always-min-source []
  (let [state (atom (initial-state))]
    (->AlwaysMinSource state)))
