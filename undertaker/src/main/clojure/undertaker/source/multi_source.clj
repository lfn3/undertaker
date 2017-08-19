(ns ^{:doc "Combines multiple other sources to cover likely corner cases"}
undertaker.source.multi-source
  (:require [undertaker.proto :as proto]
            [undertaker.source.always-max-source :as source.max]
            [undertaker.source.wrapped-random :as source.random]))

(defn next-source [state-atom]
  (let [next-source (first (::sources state-atom))]
    {::current-source next-source
     ::sources        (rest (::sources state-atom))}))

(defn initial-state [seed]
  {::current-source (source.max/make-always-max-source)
   ::sources        [(source.random/make-source seed)]})

(defrecord MultiSource [state-atom]
  proto/UnsignedByteSource
  (get-ubyte [_ ceiling] (proto/get-ubyte (::current-source @state-atom) ceiling))
  proto/Interval
  (push-interval [_ interval-name] (proto/push-interval (::current-source @state-atom) interval-name))
  (pop-interval [_ interval-id generated-value] (proto/pop-interval (::current-source @state-atom) interval-id generated-value))
  (get-intervals [_] (::completed-intervals @state-atom))
  proto/Recall
  (get-sourced-bytes [_] (proto/get-sourced-bytes (::current-source @state-atom)))
  (reset [_] (if (first (::sources @state-atom))
               (swap! state-atom next-source)
               (proto/reset (::current-source @state-atom)))))

(defn make-source [seed]
  (map->MultiSource (initial-state seed)))
