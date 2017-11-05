(ns undertaker.source.wrapped-random
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [undertaker.bytes :as bytes]
            [clojure.set :as set])
  (:import (java.util Random)))

(defn get-bytes-from-java-random [^Random rnd ^long count]
  (let [arr (byte-array count)]
    (.nextBytes rnd arr)
    arr))

(s/def ::interval-id-counter int?)
(s/def ::bytes-counter int?)
(s/def ::completed-intervals (s/coll-of ::proto/interval))
(s/def ::frozen boolean?)
(s/def ::source-state (s/keys :req [::interval-id-counter
                                    ::bytes-counter
                                    ::proto/interval-stack
                                    ::completed-intervals
                                    ::frozen]))

(defn- push-interval* [state interval-name hints]
  (let [id (inc (::interval-id-counter state))]
    (-> state
        (update ::interval-id-counter inc)
        (update ::proto/interval-stack conj {::proto/interval-name      interval-name
                                             ::proto/interval-id        id
                                             ::proto/interval-start     (get state ::bytes-counter)
                                             ::proto/interval-parent-id (-> state
                                                                            ::proto/interval-stack
                                                                            (last)
                                                                            ::proto/interval-id)
                                             ::proto/hints              hints}))))

(defn- pop-interval* [state interval-id generated-value]
  (let [interval-to-update (last (::proto/interval-stack state))]
    (when (not= (::proto/interval-id interval-to-update) interval-id)
      (throw (ex-info "Popped interval without matching id"
                      {:expected-id     interval-id
                       :popped-interval interval-to-update
                       :state           state})))
    (let [started-at (::proto/interval-start interval-to-update)
          ending-at (get state ::bytes-counter)
          length (- ending-at started-at)]
      (-> state
          (update ::proto/interval-stack pop)
          (update ::completed-intervals conj (-> interval-to-update
                                                 (assoc ::proto/interval-end ending-at)
                                                 (assoc ::proto/generated-value generated-value)
                                                 (assoc ::proto/mapped-bytes (->> state
                                                                                  ::bytes
                                                                                  (drop started-at)
                                                                                  (take length)
                                                                                  (vec)))))))))

(def initial-state {::interval-id-counter  0
                    ::bytes-counter        0
                    ::proto/interval-stack []
                    ::completed-intervals  []
                    ::frozen               false
                    ::bytes                []})

(defn hints-that-apply
  "This assumes the current interval is the last one in the wip-intervals stack"
  [wip-intervals]
  (let [immediate-children-hints (if (<= 2 (count wip-intervals))
                                   (::proto/hints (nth wip-intervals (- (count wip-intervals) 2)))
                                   [])]
    immediate-children-hints))

(defmulti apply-hint*
  (fn [wip-intervals completed-intervals ranges skip hint] (last hint)))

(defmethod apply-hint* ::proto/unique
  [wip-intervals completed-intervals ranges skip hint]
  [ranges skip])

(defmethod apply-hint* :default
  [wip-intervals completed-intervals ranges skip hint]
  (throw (IllegalArgumentException. "Can't apply hint" hint)))

(defn apply-hints [wip-intervals completed-intervals ranges skip hints]
  (loop [ranges ranges
         skip skip
         hints hints]
    (if-let [hint (first hints)]
      (let [ranges skip] (apply-hint* wip-intervals completed-intervals ranges skip hint)
                         (recur ranges skip (rest hints)))
      [ranges skip])))

(s/fdef apply-hints
  :args (s/cat :wip-intervals ::proto/interval-stack
               :completed-intervals ::completed-intervals
               :ranges ::bytes/ranges
               :skip ::bytes/bytes-to-skip
               :hint ::proto/hints)
  :ret (s/tuple ::bytes/ranges ::bytes/bytes-to-skip))

(defn get-already-generated-when-unique [wip-intervals completed-intervals]
  (let [current-interval (last wip-intervals)]
    (if-let [uniqueness-key (get current-interval ::proto/hints)]
      (->> completed-intervals
           (filter (comp (partial = uniqueness-key) ::bytes/uniqueness-key))
           (map ::proto/mapped-bytes)
           (into #{}))
      #{})))

(s/fdef get-already-generated-when-unique
  :args (s/cat :wip-intervals ::proto/interval-stack :completed-intervals ::completed-intervals)
  :ret ::bytes/bytes-to-skip)

(defrecord WrappedRandomSource
  [rnd state-atom]
  proto/ByteArraySource
  (get-bytes [this ranges skip]
    (let [unmapped (get-bytes-from-java-random rnd (-> ranges (first) (first) (count)))
          {:keys [::proto/interval-stack ::completed-intervals]} @state-atom
          [ranges skip] (->> interval-stack
                             (hints-that-apply)
                             (apply-hints interval-stack completed-intervals ranges skip))
          mapped (bytes/map-into-ranges unmapped ranges skip)]
      (swap! state-atom #(-> %1
                             (update ::bytes-counter (partial + (count mapped)))
                             (update ::bytes (fn [existing-bytes] (concat existing-bytes (vec mapped))))))
      mapped))
  proto/Interval
  (push-interval [_ interval-name hints]
    (::interval-id-counter (swap! state-atom push-interval* interval-name hints)))
  (pop-interval [_ interval-id generated-value]
    (swap! state-atom pop-interval* interval-id generated-value))
  (get-intervals [_] (::completed-intervals @state-atom))
  (get-wip-intervals [_] (::proto/interval-stack @state-atom))
  proto/Recall
  (get-sourced-bytes [_]
    (-> state-atom
        deref
        ::bytes
        (byte-array)))
  (reset [_]
    (reset! state-atom initial-state)))

(defn state-validator [state]
  (when-not (s/valid? ::source-state state)
    (throw (ex-info "Did not match spec" {:explained (s/explain ::source-state state)})))
  (when-not (= (::bytes-counter state) (count (::bytes state)))
    (throw (ex-info "Bytes counter out of step with array" {:counter (::bytes-counter state)
                                                            :bytes   (vec (::bytes state))})))
  (->> state
       (map ::completed-intervals)
       (filter #(>= (::bytes-counter state) (::proto/interval-start %1))))
  (->> state
       (map ::completed-intervals)
       (filter #(>= (::bytes-counter state) (::proto/interval-end %1)))))

(defn make-source [seed]
  (let [rnd (Random. seed)
        state (atom initial-state :validator state-validator)]
    (->WrappedRandomSource rnd state)))

(s/fdef make-source
  :args (s/cat :seed integer?)
  :ret (comp (partial extends? proto/ByteArraySource) class))
