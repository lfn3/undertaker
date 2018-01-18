(ns net.lfn3.undertaker.source.always-max
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.intervals :as intervals]
            [net.lfn3.undertaker.bytes :as bytes])
  (:import (java.nio ByteBuffer)))

(defrecord AlwaysMaxSource []
  proto/ByteArraySource
  (get-bytes [_ ranges]
    (let [flattened-ranges (mapcat identity ranges)]
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
                              :default (recur (inc idx) max-ranges))))]
          (ByteBuffer/wrap (byte-array max-range))))))
  (reset [_]))

(defn make-always-max-source []
  (->AlwaysMaxSource))
