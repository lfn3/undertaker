(ns undertaker.intervals
  (:require [undertaker.proto :as proto]
            [undertaker.bytes :as bytes]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [undertaker.debug :as debug]))

(defmulti apply-hint*
  (fn [wip-intervals completed-intervals ranges skip hint] (nth hint 1)))

(defn has-same-hint-xf [[applies-to hint-key hint-args]]
  (fn [xf]
    (let [depth (volatile! ::none)]
      (fn
        ([] (xf))
        ([result]
         (xf result))
        ([result {:keys [::proto/interval-depth ::proto/hints] :as input}]
         (let [current-depth @depth]
           (cond (= ::bytes/none depth)
                 (do (dorun (map (fn [[input-applies-to input-key input-args]]
                             (when (and (= hint-key input-key)
                                        (= hint-args input-args))
                               (vreset! depth interval-depth))) hints))
                     ))))))))

(defn has-same-hint [hint intervals]
  (let [hint-key (nth hint 1)
        hint-args (last hint)]
    (filter (fn [interval] (->> interval
                                ::proto/hints
                                (filter #(and (= hint-key (nth %1 1))
                                              (= hint-args (last %1))))
                                (seq)))
            intervals)))

(s/fdef has-same-hint
        :args (s/cat :hint ::proto/hint :intervals ::proto/completed-intervals)
        :ret ::proto/completed-intervals)

(defn get-already-generated-when-unique [hint wip-intervals completed-intervals]
  (->> (seq (has-same-hint hint completed-intervals))
       (#(do (prn %1) %1) )
       (map ::proto/mapped-bytes)
       (into #{})))

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


(defn push-interval [state hints]
  (update state ::proto/interval-stack conj {::proto/interval-start (count (::bytes/bytes state))
                                             ::proto/interval-depth (count (::proto/interval-stack state))
                                             ::proto/hints          hints}))

(s/fdef push-interval
  :args (s/cat :state ::proto/source-state :hints ::proto/hints)
  :ret ::proto/source-state)

(defn pop-interval [state generated-value]
  (let [interval-to-update (last (::proto/interval-stack state))]
    (let [started-at (::proto/interval-start interval-to-update)
          ending-at (count (get state ::bytes/bytes))
          length (- ending-at started-at)]
      (when (some #(= ::proto/unique (nth %1 1)) (hints-that-apply (::proto/interval-stack state)))
        (prn (->> state
              ::bytes/bytes
              (drop started-at)
              (take length)
              (vec))))
      (let [s (-> state
                      (update ::proto/interval-stack pop)
                      (update ::proto/completed-intervals conj
                              (cond-> interval-to-update
                                true
                                (assoc ::proto/interval-end ending-at)
                                (zero? (::proto/interval-depth interval-to-update))
                                (assoc ::proto/generated-value generated-value)
                                (some #(= ::proto/unique (nth %1 1)) (hints-that-apply (::proto/interval-stack state)))
                                (assoc ::proto/mapped-bytes (->> state
                                                                 ::bytes/bytes
                                                                 (drop started-at)
                                                                 (take length)
                                                                 (vec))))))]
        (prn s)
        s))))

(s/fdef pop-interval
  :args (s/cat :state ::proto/source-state :generated-value ::proto/generated-value)
  :ret ::proto/source-state)
