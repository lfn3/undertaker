(ns ^{:doc "Combines multiple other sources to cover likely corner cases"}
net.lfn3.undertaker.source.multi
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.source.always-max :as source.max]
            [net.lfn3.undertaker.source.wrapped-random :as source.random]
            [net.lfn3.undertaker.source.always-min :as source.zero]
            [net.lfn3.undertaker.source :as source]))

(defn next-source [state]
  (let [next-source (first (::sources state))
        current-source (::current-source state)
        current-max-size (::max-size-of-source state)
        max-size-of-source (-> (source/get-sourced-bytes current-source)
                               (.limit)
                               (max current-max-size))]
    (if next-source
      (if (fn? next-source)
        {::current-source (next-source max-size-of-source)
         ::sources        (rest (::sources state))
         ::max-size-of-source max-size-of-source}
        {::current-source next-source
         ::sources        (rest (::sources state))
         ::max-size-of-source max-size-of-source})
      state)))

(defn initial-state [seed]
  {::current-source     (source.max/make-always-max-source)
   ::sources            [(source.zero/make-always-min-source)
                         (partial source.random/make-source seed)]
   ::max-size-of-source 0})

(defrecord MultiSource [state-atom]
  proto/ByteArraySource
  (get-bytes [_ ranges] (proto/get-bytes (::current-source @state-atom) ranges))
  proto/Interval
  (push-interval [_ hints] (proto/push-interval (::current-source @state-atom) hints))
  (pop-interval [_ generated-value] (proto/pop-interval (::current-source @state-atom) generated-value))
  (get-intervals [_] (proto/get-intervals (::current-source @state-atom)))
  (get-wip-intervals [_] (proto/get-wip-intervals (::current-source @state-atom)))
  proto/Recall
  (get-sourced-bytes [_] (proto/get-sourced-bytes (::current-source @state-atom)))
  (reset [_] (if (first (::sources @state-atom))
               (swap! state-atom next-source)
               (proto/reset (::current-source @state-atom)))))

(defn make-source [seed] (->MultiSource (atom (initial-state seed))))
