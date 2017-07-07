(ns undertaker.proto
  (:require [clojure.spec.alpha :as s]))

(s/def ::interval-name string?)
(s/def ::interval-id int?)
(s/def ::interval-start int?)
(s/def ::interval-end int?)
(s/def ::generated-value (s/with-gen any? #(s/gen nil?)))
(s/def ::wip-interval (s/tuple ::interval-name ::interval-id ::interval-start))
(s/def ::interval (s/tuple ::interval-name ::interval-id ::interval-start ::interval-end ::generated-value))

(defprotocol ByteSource
  (get-byte [this]))

(defprotocol BytesSource
  (get-bytes [this number]))

(defprotocol Interval
  (push-interval [this interval-name])
  (pop-interval [this interval-id generated-value])
  (get-intervals [this]))

(defprotocol Recall
  "Allows you to get the sequence of bytes this source of randomness has emitted since the last reset."
  (get-sourced-bytes [this])
  (reset [this]))
