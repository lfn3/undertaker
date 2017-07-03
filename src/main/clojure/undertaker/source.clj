(ns undertaker.source
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s])
  (:import (java.util Random)))

(s/def ::interval-name string?)
(s/def ::interval-id int?)
(s/def ::interval-start int?)
(s/def ::interval-end int?)
(s/def ::generated-value (s/with-gen any?
                                     #(s/gen nil?)))
(s/def ::wip-interval (s/tuple ::interval-name ::interval-id ::interval-start))
(s/def ::interval (s/tuple ::interval-name ::interval-id ::interval-start ::interval-end ::generated-value))
