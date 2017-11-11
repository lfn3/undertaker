(ns undertaker.intervals
  (:require [undertaker.proto :as proto]
            [undertaker.bytes :as bytes]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(defmulti apply-hint*
  (fn [wip-intervals completed-intervals ranges skip hint] (nth hint 1)))

(defn has-same-hint [hint intervals]
  (let [hint-key (nth hint 1)
        hint-args (last hint)]
    (filter (fn [interval] (->> interval
                                ::proto/hints
                                (filter #(= hint-key (nth %1 1)))
                                (filter #(= hint-args (last %1)))
                                (seq)))
            intervals)))

(s/fdef has-same-hint
        :args (s/cat :hint ::proto/hint :intervals (s/or :wip ::proto/interval-stack
                                                         :complete ::proto/completed-intervals))
        :ret (s/or :wip ::proto/interval-stack
                   :complete ::proto/completed-intervals))

(defn get-already-generated-when-unique [hint wip-intervals completed-intervals]
  (let [current-interval (last wip-intervals)
        uniqueness-key (last hint)
        same-hint (has-same-hint hint completed-intervals)]
    (if-let [parent (get current-interval ::proto/interval-parent-id)]
      (->> same-hint
           (map ::proto/mapped-bytes)
           (into #{}))
      #{})))

(s/fdef get-already-generated-when-unique
        :args (s/cat :hint ::proto/hint :wip-intervals ::proto/interval-stack :completed-intervals ::proto/completed-intervals)
        :ret ::bytes/bytes-to-skip)

(defmethod apply-hint* ::proto/unique
  [wip-intervals completed-intervals ranges skip hint]
  [ranges (set/union skip (get-already-generated-when-unique hint wip-intervals completed-intervals))])

(defmethod apply-hint* :default
  [wip-intervals completed-intervals ranges skip hint]
  (throw (IllegalArgumentException. (str "Can't apply hint " hint))))

(defn hints-that-apply
  "This assumes the current interval is the last one in the wip-intervals stack"
  [wip-intervals]
  (let [immediate-children-hints (if (<= 2 (count wip-intervals))
                                   (::proto/hints (nth wip-intervals (- (count wip-intervals) 2)))
                                   [])]
    immediate-children-hints))

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

(s/fdef apply-hints
  :args (s/cat :wip-intervals ::proto/interval-stack
               :completed-intervals ::proto/completed-intervals
               :ranges ::bytes/ranges
               :skip ::bytes/bytes-to-skip)
  :ret (s/tuple ::bytes/ranges ::bytes/bytes-to-skip))
