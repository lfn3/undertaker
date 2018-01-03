(ns net.lfn3.undertaker.intervals
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [clojure.set :as set]))

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
  (let [{:keys [::bytes/byte-buffers ::proto/interval-stack]} state]
    (update state ::proto/interval-stack conj {::proto/interval-start-buffer (count byte-buffers)
                                               ::proto/interval-start        (bytes/length-of-buffers byte-buffers)
                                               ::proto/interval-depth        (count interval-stack)
                                               ::proto/hints                 hints})))

(defn build-completed-interval [wip-interval generated-value byte-buffers uniqueness-hint-id]
  (cond-> wip-interval
    true (assoc ::proto/interval-end (bytes/length-of-buffers byte-buffers))
    true (assoc ::proto/generated-value generated-value)
    uniqueness-hint-id (assoc ::proto/mapped-bytes (bytes/buffers->bytes byte-buffers
                                                                         (::proto/interval-start-buffer wip-interval)
                                                                         (count byte-buffers)))
    uniqueness-hint-id (assoc ::proto/uniqueness-hint-id uniqueness-hint-id)))

(defn pop-interval [state generated-value]
  (let [interval-to-update (last (::proto/interval-stack state))
        byte-buffers (::bytes/byte-buffers state)
        applicable-hints (hints-that-apply (::proto/interval-stack state))
        uniqueness-hint-id (->> applicable-hints
                                (filter #(= ::proto/unique (nth %1 1))) ;Assumes there's only one.
                                (last)
                                (last))
        top-of-intervals? (zero? (::proto/interval-depth interval-to-update))
        snippable? (some (comp (partial = ::proto/snippable) #(nth %1 1)) applicable-hints)
        keep? (or snippable? top-of-intervals? uniqueness-hint-id)]
    (cond-> state
      true (update ::proto/interval-stack pop)
      keep? (update ::proto/completed-intervals conj (build-completed-interval interval-to-update
                                                                               generated-value
                                                                               byte-buffers
                                                                               uniqueness-hint-id)))))

(defmulti apply-hint* (fn [_ _ _ _ [_ hint _]] hint))

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
  [_ _ _ _ hint]
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
