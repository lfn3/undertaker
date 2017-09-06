(ns undertaker.bytes
  (:require [undertaker.util :as util]
            [clojure.spec.alpha :as s])
  (:import (java.nio ByteBuffer)))

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
  (when (seq ranges)
    (let [with-distances (map #(cons (distance-to-range value %1) %1) ranges)
          min-distance (reduce min (map first with-distances))]
      (drop 1 (last (filter #(= min-distance (first %1)) with-distances))))))

(defn value-in-range? [value floor ceiling]
  (let [[floor ceiling] (if (util/unsigned<= ceiling floor)
                          [ceiling floor]
                          [floor ceiling])]
    (and (util/unsigned<= floor value)
         (util/unsigned<= value ceiling))))

(defn values-in-range? [values range]
  (->> (map-indexed #(value-in-range? %2
                                      (nth (first range) %1)
                                      (nth (last range) %1))
                    values)
       (every? true?)))

(defn byte-array->bits [bytes]
  (map #(.substring (Integer/toBinaryString (+ (bit-and 0xFF %1) 0x100)) 1) bytes))

(defn potentially-matched-disallowed-values [bytes disallowed-values]
  (->> disallowed-values
       (filter #(= (inc (count bytes)) (count %1)))         ;Make sure we only check against values we're about to generate
       (filter #(every? true? (map = (take (dec (count %1)) bytes) %1))) ;Check if all bar the last byte match the disallowed value.
       (map last)))

(defn map-into-ranges
  ([input ranges] (map-into-ranges input ranges #{}))
  ([input ranges skip-values]
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
               floor (if all-mins (first range) 0)          ;Probably some kind of mod thing rather than just into the
               ceiling (if all-maxes (last range) -1)       ;Closest range
               skip-values (->> skip-values
                                (potentially-matched-disallowed-values (take idx output-arr))
                                (filter #(and (util/unsigned<= floor %1) (util/unsigned<= %1 ceiling))))
               next-val (move-into-range input-val floor ceiling skip-values)]
           (aset-byte output-arr idx next-val)
           (recur (inc idx)
                  (and all-mins (some true? (map #(= next-val (first %1)) ranges-at-idx)))
                  (and all-maxes (some true? (map #(= next-val (last %1)) ranges-at-idx)))))))
     output-arr)))

(defn split-number-line-min-max-into-bytewise-min-max [floor ceiling ->bytes-fn]
  (if (or (and (zero? floor) (pos? ceiling))
          (and (pos? floor) (pos? ceiling))
          (and (neg? floor) (neg? ceiling)))
    [[(->bytes-fn floor) (->bytes-fn ceiling)]]
    [[(->bytes-fn floor) (->bytes-fn -1)] [(->bytes-fn 0) (->bytes-fn ceiling)]]))

(defn bytes->byte [arr]
  (nth arr 0))

(defn byte->bytes [b]
  (let [out (byte-array 1)]
    (aset-byte out 0 b)
    out))

(defn bytes->short [arr]
  (-> arr
      (cond-> (not (bytes? arr)) (byte-array))
      (ByteBuffer/wrap)
      (.getShort)))

(defn short->bytes [s]
  (let [out (byte-array 2)
        wrapped (ByteBuffer/wrap out)]
    (.putShort wrapped s)
    out))

(s/def ::short (s/and int?
                      #(<= Short/MIN_VALUE %1)
                      #(<= %1 Short/MAX_VALUE)))

(s/fdef bytes->short
  :args (s/cat :arr ::util/bytes)
  :ret ::short
  :fn (fn [{:keys [args ret]}]
        (->> (short->bytes ret)
             (map = (-> args
                        :arr
                        val))
             (every? true?))))

(s/fdef short->bytes
  :args (s/cat :s ::short)
  :ret ::util/bytes
  :fn (fn [{:keys [args ret]}] (= (:s args) (bytes->short ret))))

(defn bytes->int [arr]
  (-> arr
      (cond-> (not (bytes? arr)) (byte-array))
      (ByteBuffer/wrap)
      (.getInt)))

(defn int->bytes [^Integer i]
  (let [out (byte-array 4)
        wrapped (ByteBuffer/wrap out)]
    (.putInt wrapped i)
    out))

(s/fdef int->bytes
  :args (s/cat :i integer?)
  :ret bytes?)

(defn bytes->long [arr]
  (-> arr
      (cond-> (not (bytes? arr)) (byte-array))
      (ByteBuffer/wrap)
      (.getLong)))

(defn long->bytes [^Long i]
  (let [out (byte-array 8)
        wrapped (ByteBuffer/wrap out)]
    (.putLong wrapped i)
    out))

(defn bytes->double [arr]
  (-> arr
      (cond-> (not (bytes? arr)) (byte-array))
      (ByteBuffer/wrap)
      (.getDouble)))

(defn double->bytes [^Double d]
  (let [out (byte-array 8)
        wrapped (ByteBuffer/wrap out)]
    (.putDouble wrapped d)
    out))