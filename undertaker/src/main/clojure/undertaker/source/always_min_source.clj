(ns undertaker.source.always-min-source
  (:require [undertaker.proto :as proto]
            [undertaker.bytes :as bytes]
            [undertaker.intervals :as intervals]))

(defn- push-interval* [state interval-name hints]
  (let [id (inc (::interval-id-counter state))]
    (-> state
        (update ::interval-id-counter inc)
        (update ::proto/interval-stack conj {::proto/interval-name      interval-name
                                             ::proto/interval-id        id
                                             ::proto/interval-start     (count (get state ::bytes))
                                             ::proto/interval-parent-id (-> state
                                                                            ::proto/interval-stack
                                                                            (last)
                                                                            ::proto/interval-id)
                                             ::proto/hints hints}))))

(defn- pop-interval* [state interval-id generated-value]
  (let [interval-to-update (last (::proto/interval-stack state))]
    (when (not= (::proto/interval-id interval-to-update) interval-id)
      (throw (ex-info "Popped interval without matching id"
                      {:expected-id     interval-id
                       :popped-interval interval-to-update
                       :state           state})))
    (let [started-at (get interval-to-update ::proto/interval-start)
          ending-at (count (get state ::bytes))
          length (- ending-at started-at)]
      (-> state
          (update ::proto/interval-stack pop)
          (update ::proto/completed-intervals conj (-> interval-to-update
                                                 (assoc ::proto/interval-end ending-at)
                                                 (assoc ::proto/generated-value generated-value)
                                                 (assoc ::proto/mapped-bytes (->> state
                                                                                  ::bytes
                                                                                  (drop started-at)
                                                                                  (take length)
                                                                                  (vec)))))))))

(def initial-state {::interval-id-counter  0
                    ::bytes                []
                    ::proto/interval-stack []
                    ::proto/completed-intervals  []})

(defrecord AlwaysMinSource [state-atom]
  proto/ByteArraySource
  (get-bytes [_ ranges skip]
    (let [{:keys [::proto/interval-stack ::proto/completed-intervals]} @state-atom
          [ranges skip] (intervals/apply-hints interval-stack completed-intervals ranges skip)
          ranges (bytes/punch-skip-values-out-of-ranges skip ranges)
          flattened-ranges (mapcat identity ranges)]
      (if (every? nil? (map seq flattened-ranges))          ;i.e. range of size zero
        (byte-array 0)
        (let [min-range (loop [idx 0
                           ranges flattened-ranges]
                      (let [min-value (->> ranges
                                           (map #(nth %1 idx))
                                           (reduce min))
                            min-ranges (->> ranges
                                            (filter #(= min-value (nth %1 idx))))]
                        (cond
                          (= 0 (count min-ranges)) (first ranges)
                          (= 1 (count min-ranges)) (first min-ranges)
                          (< (inc idx) (count (last ranges))) (first min-ranges)
                          :default (recur (inc idx) min-ranges))))]
      (swap! state-atom update ::bytes #(concat %1 (vec min-range)))
      (byte-array min-range)))))
  proto/Interval
  (push-interval [_ interval-name hints]
    (::interval-id-counter (swap! state-atom push-interval* interval-name hints)))
  (pop-interval [_ interval-id generated-value]
    (swap! state-atom pop-interval* interval-id generated-value))
  (get-intervals [_] (::proto/completed-intervals @state-atom))
  (get-wip-intervals [_] (::proto/interval-stack @state-atom))
  proto/Recall
  (get-sourced-bytes [_]
    (-> state-atom
        deref
        ::bytes
        (byte-array)))
  (reset [_]
    (reset! state-atom initial-state)))

(defn make-always-min-source []
  (let [state (atom initial-state)]
    (->AlwaysMinSource state)))
