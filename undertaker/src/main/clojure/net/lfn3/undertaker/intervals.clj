(ns net.lfn3.undertaker.intervals
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [clojure.set :as set]
            [net.lfn3.undertaker.debug :as debug])
  (:import (net.lfn3.undertaker ChainedByteBuffer)))

(defn hints-that-apply
  "This assumes the current interval is the last one in the wip-intervals stack"
  [wip-intervals]
  (let [this-hints (->> wip-intervals
                        last
                        ::proto/hints
                        (filter (comp (partial = ::proto/this) first)))
        immediate-children-hints (if (<= 2 (count wip-intervals))
                                   (->> (- (count wip-intervals) 2)
                                        (nth wip-intervals)
                                        ::proto/hints
                                        (filter (comp (partial = ::proto/immediate-children-of) first)))
                                   [])]
    (concat this-hints immediate-children-hints)))

(defn push-interval [state hints]
  (let [^ChainedByteBuffer chained-byte-buffer (::bytes/chained-byte-buffer state)]
    (update state ::proto/interval-stack conj {::proto/interval-start (.limit chained-byte-buffer)
                                               ::proto/interval-depth (count (::proto/interval-stack state))
                                               ::proto/hints          hints})))

(defn pop-interval [state generated-value]
  (let [interval-to-update (last (::proto/interval-stack state))]
    (let [started-at (::proto/interval-start interval-to-update)
          ^ChainedByteBuffer chained-buffer (::bytes/chained-byte-buffer state)
          ending-at (.limit chained-buffer)
          length (- ending-at started-at)
          applicable-hints (hints-that-apply (::proto/interval-stack state))
          uniqueness-hint-id (->> applicable-hints
                                  (filter #(= ::proto/unique (nth %1 1))) ;Assumes there's only one.
                                  (last)
                                  (last))
          top-level-interval? (zero? (::proto/interval-depth interval-to-update))
          snippable? (some (comp (partial = ::proto/snippable) #(nth %1 1)) applicable-hints)
          keep? (or snippable? top-level-interval? uniqueness-hint-id)]
      (cond-> state
              true (update ::proto/interval-stack pop)
              keep? (update ::proto/completed-intervals conj
                            (cond-> interval-to-update
                                    true
                                    (assoc ::proto/interval-end ending-at)
                                    top-level-interval?
                                    (assoc ::proto/generated-value generated-value)
                                    uniqueness-hint-id
                                    (assoc ::proto/mapped-bytes (-> chained-buffer
                                                                    (.last)
                                                                    (.array)))
                                    uniqueness-hint-id
                                    (assoc ::proto/uniqueness-hint-id uniqueness-hint-id)))))))

(defmulti apply-hint*
          (fn [wip-intervals completed-intervals ranges skip hint] (nth hint 1)))

(defn get-already-generated-when-unique [[_ _ uniqueness-id] wip-intervals completed-intervals]
  (->> completed-intervals
       (filter #(= uniqueness-id (::proto/uniqueness-hint-id %1)))
       (seq)
       (map ::proto/mapped-bytes)
       (into #{})))

(defmethod apply-hint* ::proto/unique
  [wip-intervals completed-intervals ranges skip hint]
  [ranges (set/union skip (get-already-generated-when-unique hint wip-intervals completed-intervals))])

(defmethod apply-hint* :default
  [wip-intervals completed-intervals ranges skip hint]
  (throw (IllegalArgumentException. (str "Can't apply hint " hint))))

(defn apply-hints [wip-intervals completed-intervals ranges skip]
  "Picks hints off the wip-intervals stack"
  (let [hints (hints-that-apply wip-intervals)]
    (loop [ranges ranges
           skip skip
           hints hints]
      (if-let [hint (first hints)]
        (let [[ranges skip] (apply-hint* wip-intervals completed-intervals ranges skip hint)]
          (recur ranges skip (rest hints)))
        [ranges skip]))))
