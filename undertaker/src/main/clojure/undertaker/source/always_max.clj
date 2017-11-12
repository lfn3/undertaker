(ns undertaker.source.always-max
  (:require [undertaker.proto :as proto]
            [undertaker.intervals :as intervals]
            [undertaker.bytes :as bytes]))

(def initial-state {::proto/interval-id-counter 0
                    ::bytes/bytes               []
                    ::proto/interval-stack      []
                    ::proto/completed-intervals []})

(defrecord AlwaysMaxSource [state-atom]
  proto/ByteArraySource
  (get-bytes [_ ranges skip]
    (let [{:keys [::proto/interval-stack ::proto/completed-intervals]} @state-atom
          [ranges skip] (intervals/apply-hints interval-stack completed-intervals ranges skip)
          ranges (bytes/punch-skip-values-out-of-ranges skip ranges)
          flattened-ranges (mapcat identity ranges)]
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
          (swap! state-atom update ::bytes/bytes #(concat %1 (vec max-range)))
          (byte-array max-range)))))
  proto/Interval
  (push-interval [_ interval-name hints]
    (::proto/interval-id-counter (swap! state-atom intervals/push-interval interval-name hints)))
  (pop-interval [_ interval-id generated-value]
    (swap! state-atom intervals/pop-interval interval-id generated-value))
  (get-intervals [_] (::proto/completed-intervals @state-atom))
  (get-wip-intervals [_] (::proto/interval-stack @state-atom))
  proto/Recall
  (get-sourced-bytes [_]
    (-> state-atom
        deref
        ::bytes/bytes
        (byte-array)))
  (reset [_]
    (reset! state-atom initial-state)))

(defn make-always-max-source []
  (let [state (atom initial-state)]
    (->AlwaysMaxSource state)))
