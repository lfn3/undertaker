(ns undertaker.proto
  (:require [clojure.spec.alpha :as s]))

(s/def ::interval-name string?)
(s/def ::interval-parent-id (s/or :nil nil? :id int?))
(s/def ::interval-id int?)
(s/def ::interval-start int?)
(s/def ::interval-end int?)
(s/def ::generated-value (s/with-gen any? #(s/gen nil?)))
(s/def ::wip-interval (s/keys :req [::interval-name ::interval-id ::interval-start]
                              :opt [::interval-parent-id]))
(s/def ::interval (s/keys :req [::interval-name ::interval-id ::interval-start ::interval-end ::generated-value]
                          :opt [::interval-parent-id]))

(s/def ::interval-stack (s/coll-of ::wip-interval))

(defprotocol UnsignedByteSource
  (get-ubyte [this max]))

(defprotocol ByteArraySource
  (get-bytes [this ranges skip]))

(defprotocol Interval
  (push-interval [this interval-name])
  (pop-interval [this interval-id generated-value])
  (get-intervals [this])
  (get-wip-intervals [this]))

(defprotocol Recall
  "Allows you to get the sequence of bytes this source of randomness has emitted since the last reset."
  (get-sourced-bytes [this])
  (reset [this]))
