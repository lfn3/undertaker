(ns ^{:doc "Combines multiple other sources to cover likely corner cases"}
undertaker.source.multi-source
  (:require [undertaker.proto :as proto]
            [undertaker.source.always-max-source :as source.max]
            [undertaker.source.wrapped-random :as source.random]
            [undertaker.source.always-min-source :as source.zero]))

(defn next-source [state]
  (let [next-source (first (::sources state))]
    (if next-source
      {::current-source next-source
       ::sources        (rest (::sources state))}
      state)))

(defn initial-state [seed]
  {::current-source (source.max/make-always-max-source)
   ::sources        [(source.zero/make-always-zero-source)
                     (source.random/make-source seed)]})

(defrecord MultiSource [state-atom]
  proto/ByteArraySource
  (get-bytes [_ ranges skip] (proto/get-bytes (::current-source @state-atom) ranges skip))
  proto/Interval
  (push-interval [_ interval-name] (proto/push-interval (::current-source @state-atom) interval-name))
  (pop-interval [_ interval-id generated-value] (proto/pop-interval (::current-source @state-atom) interval-id generated-value))
  (get-intervals [_] (proto/get-intervals (::current-source @state-atom)))
  (get-wip-intervals [_] (proto/get-wip-intervals (::current-source @state-atom)))
  proto/Recall
  (get-sourced-bytes [_] (proto/get-sourced-bytes (::current-source @state-atom)))
  (reset [_] (if (first (::sources @state-atom))
               (swap! state-atom next-source)
               (proto/reset (::current-source @state-atom)))))

(defn make-source [seed]
  (->MultiSource (atom (initial-state seed))))
