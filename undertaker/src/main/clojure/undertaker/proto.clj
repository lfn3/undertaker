(ns undertaker.proto
  (:require [clojure.spec.alpha :as s]
            [undertaker.bytes :as bytes]
            [undertaker.debug :as debug]))

(s/def ::interval-start (s/or :pos pos-int? :zero zero?))
(s/def ::interval-depth (s/or :pos pos-int? :zero zero?))
(s/def ::hint-applies-to #{::immediate-children-of})
(s/def ::hint-names #{::unique})
(s/def ::hint-args any?)
(s/def ::hint (s/tuple ::hint-applies-to ::hint-names ::hint-args))
(s/def ::hints (s/coll-of ::hint))

(s/def ::wip-interval (s/keys :req [::interval-start ::hints ::interval-depth]))

(s/def ::interval-end (s/or :pos pos-int? :zero zero?))
(s/def ::generated-value (s/with-gen any? #(s/gen nil?)))
(s/def ::mapped-bytes ::bytes/bytes)

(s/def ::interval (s/keys :req [::interval-start
                                ::interval-end
                                ::generated-value
                                ::mapped-bytes
                                ::hints]))

(s/def ::interval-stack (s/coll-of ::wip-interval))
(s/def ::completed-intervals (s/coll-of ::interval))

(s/def ::source-state (s/keys :req [::interval-stack
                                    ::completed-intervals
                                    ::bytes/bytes]))

(defn source-state-validator [state]
  (let [byte-counter (count (::bytes/bytes state))]
    (when-not (s/valid? ::source-state state)
      (throw (debug/internal-exception "Did not match spec" {:explained (s/explain ::source-state state)})))

    (when-let [overrunning-intervals (->> state
                                          ::completed-intervals
                                          (filter #(< byte-counter (::interval-start %1)))
                                          (seq))]
      (throw (debug/internal-exception "Intervals overran generated bytes" {:intervals    overrunning-intervals
                                                                            :byte-counter byte-counter})))
    (when-let [overrunning-intervals (->> state
                                          ::completed-intervals
                                          (filter #(< byte-counter (::interval-end %1)))
                                          (seq))]
      (throw (debug/internal-exception "Intervals overran generated bytes" {:intervals    overrunning-intervals
                                                                            :byte-counter byte-counter})))
    true))

(defprotocol ByteArraySource
  (get-bytes [this ranges skip]))

(defprotocol Interval
  (push-interval [this hints])
  (pop-interval [this generated-value])
  (get-intervals [this])
  (get-wip-intervals [this]))

(defprotocol Recall
  "Allows you to get the sequence of bytes this source of randomness has emitted since the last reset."
  (get-sourced-bytes [this])
  (reset [this]))
