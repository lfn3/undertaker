(ns undertaker.proto
  (:require [clojure.spec.alpha :as s]
            [undertaker.bytes :as bytes]))

(s/def ::interval-name string?)
(s/def ::interval-parent-id (s/or :nil nil? :id int?))
(s/def ::interval-id int?)
(s/def ::interval-start (s/or :pos pos-int? :zero zero?))
(s/def ::hint-applies-to #{::immediate-children-of})
(s/def ::hint-names #{::unique})
(s/def ::hint (s/tuple ::hint-applies-to ::hint-names))
(s/def ::hints (s/coll-of ::hint))

(s/def ::wip-interval (s/keys :req [::interval-name ::interval-id ::interval-start ::hints]
                              :opt [::interval-parent-id]))

(s/def ::interval-end (s/or :pos pos-int? :zero zero?))
(s/def ::generated-value (s/with-gen any? #(s/gen nil?)))
(s/def ::mapped-bytes ::bytes/bytes)

(s/def ::interval (s/keys :req [::interval-name ::interval-id ::interval-start ::interval-end ::generated-value ::mapped-bytes]
                          :opt [::interval-parent-id ::hints]))

(s/def ::interval-stack (s/coll-of ::wip-interval))

(defprotocol ByteArraySource
  (get-bytes [this ranges skip]))

(defprotocol Interval
  (push-interval [this interval-name hints])
  (pop-interval [this interval-id generated-value])
  (get-intervals [this])
  (get-wip-intervals [this]))

(defprotocol Recall
  "Allows you to get the sequence of bytes this source of randomness has emitted since the last reset."
  (get-sourced-bytes [this])
  (reset [this]))
