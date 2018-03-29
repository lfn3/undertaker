(ns net.lfn3.undertaker.intervals
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [clojure.set :as set]))

(defn hints-that-apply
  "This assumes the current interval is the last one in the wip-intervals stack"
  [wip-intervals]
  (->> wip-intervals
       last
       ::proto/hints))

(defn push-interval [state hints]
  (let [{:keys [::bytes/byte-buffers ::proto/interval-stack :net.lfn3.undertaker.source/bytes-requested]} state
        hints (concat hints (::proto/hints-for-next-interval state))
        interval-depth (count interval-stack)
        interval (if (or (seq hints) (zero? interval-depth))
                   {::proto/interval-start-buffer (count byte-buffers)
                    ::proto/interval-start        bytes-requested
                    ::proto/interval-depth        interval-depth
                    ::proto/hints                 hints}
                   nil)]
    (-> state
        (update ::proto/interval-stack conj interval)
        (assoc ::proto/hints-for-next-interval []))))

(defn build-completed-interval [wip-interval generated-value byte-buffers uniqueness-hint-id bytes-requested]
  (cond-> wip-interval
    true (assoc ::proto/interval-end bytes-requested)
    true (assoc ::proto/generated-value generated-value)
    uniqueness-hint-id (assoc ::proto/mapped-bytes (bytes/buffers->bytes byte-buffers
                                                                         (::proto/interval-start-buffer wip-interval)
                                                                         (count byte-buffers)))
    uniqueness-hint-id (assoc ::proto/uniqueness-hint-id uniqueness-hint-id)))

(defn pop-interval [state generated-value]
  (let [{:keys [:net.lfn3.undertaker.source/bytes-requested ::proto/interval-stack ::bytes/byte-buffers]} state
        interval-to-update (last interval-stack)
        applicable-hints (hints-that-apply interval-stack)
        uniqueness-hint-id (->> applicable-hints
                                (filter #(= ::proto/unique (first %1))) ;Assumes there's only one.
                                (last)
                                (last))]
    (cond-> state
      true (update ::proto/interval-stack pop)
      (not (nil? interval-to-update)) (update ::proto/completed-intervals conj (build-completed-interval interval-to-update
                                                                                                         generated-value
                                                                                                         byte-buffers
                                                                                                         uniqueness-hint-id
                                                                                                         bytes-requested)))))

(defmulti apply-hint* (fn [_ _ _ [hint _]] hint))

(defn get-already-generated-when-unique [[_ uniqueness-id] wip-intervals completed-intervals]
  (->> completed-intervals
       (filter #(= uniqueness-id (::proto/uniqueness-hint-id %1)))
       (seq)
       (map ::proto/mapped-bytes)
       (into #{})))

(defmethod apply-hint* ::proto/unique
  [wip-intervals completed-intervals ranges hint]
  (-> (get-already-generated-when-unique hint wip-intervals completed-intervals)
      (bytes/punch-skip-values-out-of-ranges ranges)))

(defmethod apply-hint* :default
  [_ _ _ _ hint]
  (throw (IllegalArgumentException. (str "Can't apply hint " hint))))

(defn apply-hints [wip-intervals completed-intervals ranges]
  "Picks hints off the wip-intervals stack"
  (let [hints (hints-that-apply wip-intervals)]
    (loop [ranges ranges
           hints hints]
      (if-let [hint (first hints)]
        (let [ranges (apply-hint* wip-intervals completed-intervals ranges hint)]
          (recur ranges (rest hints)))
        ranges))))

(defn printable-ranges
  "Transforms the byte arrays inside ranges into a vector"
  [ranges]
  (->> ranges
       (map (partial map vec))
       (map vec)
       vec))
