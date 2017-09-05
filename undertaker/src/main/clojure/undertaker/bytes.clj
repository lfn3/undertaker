(ns undertaker.bytes
  (:require [undertaker.util :as util]))

(defn unsign [b] (bit-and 0xff b))

(defn skip-disallowed-values
  [generated-byte disallowed-values]
  ;We have to do this repeatedly since we might have pushed the value up into another disallowed value.
  (let [disallowed-values (set (map unsign disallowed-values))]
    (loop [last-altered-val generated-byte]
      (let [next-altered-val (if (disallowed-values last-altered-val)
                               (inc last-altered-val)
                               last-altered-val)]
        (if (= next-altered-val last-altered-val)
          (unchecked-byte last-altered-val)
          (recur next-altered-val))))))

(defn unsigned-range [floor ceiling]
  (- (unsign ceiling) (unsign floor)))

(defn move-into-range
  ([b floor ceiling] (move-into-range b floor ceiling #{}))
  ([b floor ceiling skip-values]
   (let [[floor ceiling] (if (util/unsigned<= floor ceiling) [floor ceiling] [ceiling floor])
         unchecked-floor (unsign floor)
         range (- (unsigned-range floor ceiling) (count skip-values))]
     (cond
       (and (util/unsigned<= floor b)
            (util/unsigned<= b ceiling)                          ;If the value is within the range,
            (not ((set skip-values) b))) b                  ;just pass it through unchanged.
       (zero? range) floor
       :default (-> (unsign b)
                    (mod (inc range))
                    (+ unchecked-floor)
                    (skip-disallowed-values skip-values))))))

(defn is-in-range [value range]
  (and (util/unsigned<= (first range) value)
       (util/unsigned<= value (last range))))

(defn is-in-ranges [value ranges]
  (->> ranges
       (filter (partial is-in-range value))
       (seq)))

(defn distance-to-range [value range]
  (min (util/abs (- (unsign value) (unsign (first range))))
       (util/abs (- (unsign value) (unsign (last range))))))

(defn closest-range [value ranges]
  (let [with-distances (map #(cons (distance-to-range value %1) %1) ranges)
        min-distance (apply min (map first with-distances))]
    (drop 1 (last (filter #(= min-distance (first %1)) with-distances)))))

(defn values-in-range? [values range]
  (->> (map-indexed #(and (util/unsigned<= (nth (first range) %1) %2)
                          (util/unsigned<= %2 (nth (last range) %1)))
                    values)
       (every? true?)))

(defn potentially-matched-disallowed-values [bytes disallowed-values]
  (->> disallowed-values
       (filter #(= (inc (count bytes)) (count %1)))         ;Make sure we only check against values we're about to generate
       (filter #(every? true? (map = (take (dec (count %1)) bytes) %1))) ;Check if all bar the last byte match the disallowed value.
       (map last)))

(defn map-into-ranges [input ranges skip-values]
  (let [size (count input)
        output-arr (byte-array size)]
    (loop [idx 0
           all-mins true
           all-maxes true]
      (when (< idx size)
        (let [input-val (aget input idx)
              ranges (filter (partial values-in-range? (take idx output-arr)) ranges)
              ranges-at-idx (map (partial map #(nth %1 idx)) ranges)
              range (or (last (is-in-ranges input-val ranges-at-idx))
                        (closest-range input-val ranges-at-idx)) ;TODO: change this so it isn't as biased.
              floor (if all-mins (first range) 0)           ;Probably some kind of mod thing rather than just into the
              ceiling (if all-maxes (last range) -1)        ;Closest range
              skip-values (->> skip-values
                               (potentially-matched-disallowed-values (take idx output-arr))
                               (filter #(and (util/unsigned<= floor %1) (util/unsigned<= %1 ceiling))))
              next-val (move-into-range input-val floor ceiling skip-values)]
          (aset-byte output-arr idx next-val)
          (recur (inc idx)
                 (and all-mins (some true? (map #(= next-val (first %1)) ranges-at-idx)))
                 (and all-maxes (some true? (map #(= next-val (last %1)) ranges-at-idx)))))))
    output-arr))

(defn split-number-line-min-max-into-bytewise-min-max [floor ceiling ->bytes-fn]
  (if (or (and (= 0 (Integer/signum floor))
               (not= -1 (Integer/signum ceiling)))
          (= (Integer/signum floor) (Integer/signum ceiling)))
    [[(->bytes-fn floor) (->bytes-fn ceiling)]]
    [[(->bytes-fn floor) (->bytes-fn -1)] [(->bytes-fn 0) (->bytes-fn ceiling)]]))
