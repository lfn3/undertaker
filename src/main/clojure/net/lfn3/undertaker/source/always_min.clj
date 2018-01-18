(ns net.lfn3.undertaker.source.always-min
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.intervals :as intervals])
  (:import (java.nio ByteBuffer)))

(defrecord AlwaysMinSource []
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
                              :default (recur (inc idx) min-ranges))))]
          (ByteBuffer/wrap (byte-array min-range))))))
  (reset [_]))

(defn make-always-min-source [] (->AlwaysMinSource))
