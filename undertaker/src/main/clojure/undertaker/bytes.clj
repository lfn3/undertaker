(ns undertaker.bytes
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen])
  (:import (java.nio ByteBuffer)))

(defn unsign [b] (bit-and 0xff b))

(defn byte? [b]
  (and (integer? b)
       (>= Byte/MAX_VALUE b)
       (<= Byte/MIN_VALUE b)))

(s/fdef byte?
  :args (s/cat :b number?)
  :ret boolean?)

(s/def ::byte (s/with-gen
                (s/and integer?
                       byte?)
                #(s/gen (set (range Byte/MIN_VALUE Byte/MAX_VALUE)))))

(s/def ::bytes (s/or :arr bytes?
                     :coll (s/coll-of ::byte)))

(s/def ::bytes-to-skip (s/with-gen (s/and (s/coll-of ::bytes) set?)
                                   #(s.gen/fmap set (s/gen (s/coll-of ::bytes)))))
(s/def ::range (s/tuple ::bytes ::bytes))
(s/def ::ranges (s/coll-of ::range))
(s/def ::sliced-range (s/tuple ::byte ::byte))
(s/def ::sliced-ranges (s/coll-of ::sliced-range))

(defn abs [i]
  (if (neg-int? i) (- i) i))

(s/fdef abs
  :args (s/cat :i integer?)
  :ret (s/or :pos pos-int? :zero zero?))

(defn unsigned<= [x y]
  (not= 1 (Long/compareUnsigned x y)))

(s/fdef unsigned<=
  :args (s/cat :x int? :y int?)
  :ret boolean?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [x y]} args]
          (if (= (Long/signum x) (Long/signum y))
            (= ret (<= x y))
            (= ret (or (zero? x) (neg? y)))))))

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
   (let [[floor ceiling] (if (unsigned<= floor ceiling) [floor ceiling] [ceiling floor])
         unchecked-floor (unsign floor)
         range (- (unsigned-range floor ceiling) (count skip-values))]
     (cond
       (and (unsigned<= floor b)
            (unsigned<= b ceiling)                          ;If the value is within the range,
            (not ((set skip-values) b))) b                  ;just pass it through unchanged.
       (zero? range) floor
       :default (-> (unsign b)
                    (mod (inc range))
                    (+ unchecked-floor)
                    (skip-disallowed-values skip-values))))))

(s/fdef move-into-range
  :args (s/cat :b ::byte :floor ::byte :ceiling ::byte :skip-values (s/? ::bytes))
  :ret ::byte)

(defn is-in-range [value range]
  (and (unsigned<= (first range) value)
       (unsigned<= value (last range))))

(s/fdef is-in-range
  :args (s/cat :value ::byte :range ::sliced-range)
  :ret boolean?)

(defn is-in-ranges [value ranges]
  (->> ranges
       (filter (partial is-in-range value))
       (seq)))

(s/fdef is-in-ranges
  :args (s/cat :value ::byte :ranges ::sliced-ranges)
  :ret (s/nilable ::sliced-ranges))

(defn distance-to-range [value range]
  (min (abs (- (unsign value) (unsign (first range))))
       (abs (- (unsign value) (unsign (last range))))))

(defn closest-range [value ranges]
  (when (seq ranges)
    (let [with-distances (map #(cons (distance-to-range value %1) %1) ranges)
          min-distance (reduce min (map first with-distances))]
      (->> with-distances
           (filter #(= min-distance (first %1)))
           (last)
           (drop 1)
           (vec)))))

(s/fdef closest-range
  :args (s/cat :value ::byte :ranges ::sliced-ranges)
  :ret (s/nilable ::sliced-range))

(defn pick-range [value ranges]
  (when (seq ranges)
    (nth ranges (mod value (count ranges)))))

(s/fdef pick-range
  :args (s/cat :value ::byte :ranges ::sliced-ranges)
  :ret (s/nilable ::sliced-range))

(defn value-in-range? [value floor ceiling]
  (let [[floor ceiling] (if (unsigned<= ceiling floor)
                          [ceiling floor]
                          [floor ceiling])]
    (and (unsigned<= floor value)
         (unsigned<= value ceiling))))

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

(defn slice-ranges [idx ranges]
  (->> ranges
       (map (partial map #(nth %1 idx)))
       (map vec)))

(s/fdef slice-ranges
  :args (s/cat :idx int? :ranges ::ranges)
  :ret ::sliced-ranges)

(defn punch-skip-value-out-of-range [sliced-ranges skip-value]
  (let [ret [[(first sliced-ranges) (dec skip-value)] [(inc skip-value) (last sliced-ranges)]]]
    (->> ret
         (filter (comp not (partial reduce >))))))

(s/fdef punch-skip-value-out-of-range
  :args (s/cat :range ::sliced-range :skip-value ::byte)
  :ret ::sliced-ranges
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [range skip-value]} args]
          (if (some (partial = skip-value) range)
            (= 1 (count ret))
            (= 2 (count ret))))))

(defn punch-skip-value-out-if-in-range [sliced-range skip-value]
  (if (is-in-range skip-value sliced-range)
    (punch-skip-value-out-of-range sliced-range skip-value)
    [sliced-range]))

(s/fdef punch-skip-value-out-if-in-range
  :args (s/cat :ranges ::sliced-range :skip ::byte)
  :ret ::sliced-ranges)

(defn punch-skip-values-out-of-ranges
  [sliced-skip-bytes sliced-ranges]
  (loop [sliced-ranges sliced-ranges
         sliced-skip-bytes sliced-skip-bytes]
    (if-let [skip (first sliced-skip-bytes)]
      (recur (->> sliced-ranges
                  (mapcat #(punch-skip-value-out-if-in-range %1 skip)))
             (rest sliced-skip-bytes))
      (vec sliced-ranges))))

(s/fdef punch-skip-values-out-of-ranges
  :args (s/cat :skip ::bytes :ranges ::sliced-ranges)
  :ret ::sliced-ranges)

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
               skip-values (potentially-matched-disallowed-values (take idx output-arr) skip-values)
               ranges (->> (filter (partial values-in-range? (take idx output-arr)) ranges)
                           (slice-ranges idx)
                           (punch-skip-values-out-of-ranges skip-values))
               range (or (last (is-in-ranges input-val ranges))
                         (pick-range input-val ranges)) ;TODO: change this so it isn't as biased.
               floor (if all-mins (first range) 0)          ;Probably some kind of mod thing rather than just into the
               ceiling (if all-maxes (last range) -1)       ;Closest range TODO: some kind of short circuit based on all-mins and all-maxes?
               next-val (move-into-range input-val floor ceiling)]
           (aset-byte output-arr idx next-val)
           (recur (inc idx)
                  (and all-mins (some true? (map #(= next-val (first %1)) ranges)))
                  (and all-maxes (some true? (map #(= next-val (last %1)) ranges)))))))
     output-arr)))

(s/fdef map-into-ranges
  :args (s/cat :input ::bytes :ranges ::ranges :skip-values (s/? ::bytes-to-skip))
  :ret ::bytes
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [input ranges skip-values]} args]
          (not (skip-values ret)))))

(defn split-number-line-min-max-into-bytewise-min-max [floor ceiling ->bytes-fn]
  (if (or (and (zero? floor) (pos? ceiling))
          (and (pos? floor) (pos? ceiling))
          (and (neg? floor) (neg? ceiling)))
    [[(->bytes-fn floor) (->bytes-fn ceiling)]]
    [[(->bytes-fn floor) (->bytes-fn -1)] [(->bytes-fn 0) (->bytes-fn ceiling)]]))

(s/fdef split-number-line-min-max-into-bytewise-min-max
  :args (s/cat :floor number? :ceiling number? :->bytes-fn fn?)
  :ret ::ranges)

(defn split-number-line-ranges-into-bytewise-min-max
  ([ranges ->bytes-fn]
   (->> ranges
        (partition 2)
        (mapcat (fn [[floor ceiling]] (split-number-line-min-max-into-bytewise-min-max floor ceiling ->bytes-fn))))))

(s/fdef split-number-line-ranges-into-bytewise-min-max
  :args (s/cat :ranges (s/and (s/coll-of number?) (comp even? count))
               :->bytes-fn fn?)
  :ret ::ranges)

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
  :args (s/cat :arr ::bytes)
  :ret ::short
  :fn (fn [{:keys [args ret]}]
        (->> (short->bytes ret)
             (map = (-> args
                        :arr
                        val))
             (every? true?))))

(s/fdef short->bytes
  :args (s/cat :s ::short)
  :ret ::bytes
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

(defn bytes->float [arr]
  (-> arr
      (cond-> (not (bytes? arr)) (byte-array))
      (ByteBuffer/wrap)
      (.getFloat)))

(defn float->bytes [^Float f]
  (let [out (byte-array 4)
        wrapped (ByteBuffer/wrap out)]
    (.putFloat wrapped f)
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
