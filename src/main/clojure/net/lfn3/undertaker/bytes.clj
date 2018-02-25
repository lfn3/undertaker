(ns net.lfn3.undertaker.bytes
  (:import (java.nio ByteBuffer)
           (java.util Collection)))

(defn unsign [b] (bit-and 0xff b))

(defn byte? [b]
  (and (integer? b)
       (>= Byte/MAX_VALUE b)
       (<= Byte/MIN_VALUE b)))

(defn range<xf []
  (fn [xf]
    (fn
      ([] (xf))
      ([result]
       (if (boolean? result)
         (xf result)
         (xf false)))
      ([result [b1 b2]]
       (cond
         (= b1 b2) result
         (< b1 b2) (reduced true)
         :default (reduced false))))))

(defn range< [range1 range2]
  (->> (map (fn [v1 v2] [v1 v2]) (last range1) (first range2))
       (transduce (range<xf) identity [])))

;This exists so I can test this and the ranges-are-sorted-fn without tearing my hair out.
(defn conformed-range< [range1 range2]
  (range< (val range1) (val range2)))

(defn ranges-are-sorted-xf [range<fn]
  (fn [xf]
    (let [last-range (volatile! ::none)]
      (fn
        ([] (xf))
        ([result] (if (boolean? result)
                    (xf result)
                    (xf true)))
        ([result input]
         (let [prior @last-range]
           (vreset! last-range input)
           (if (or (= ::none prior)
                   (range<fn prior input))
             result
             (reduced false))))))))

(defn ranges-are-sorted?
  ([ranges] (ranges-are-sorted-xf conformed-range<))
  ([ranges range<fn] (transduce (ranges-are-sorted-xf range<fn) identity [] ranges)))

(defn ranges-have-same-length?
  ([ranges]
   (let [counts (->> ranges
                     (mapcat (juxt first last))
                     (map count))]
     (every? (partial = (first counts)) counts))))

(defn conformed-ranges-have-same-length? [ranges]
  (ranges-have-same-length? (map (partial map val) ranges)))

(defn negative-range? [range]
  (and (neg-int? (first (first range)))
       (neg-int? (first (last range)))))

(defn unsigned<= [x y]
  (not= 1 (Long/compareUnsigned x y)))

(defn unsigned-range [floor ceiling]
  (- (unsign ceiling) (unsign floor)))

(defn checked-byte [b]
  (if (and (<= 0 b) (<= b 255))
    (unchecked-byte b)
    (throw (IllegalArgumentException. (str "Unsigned byte should be between 0 and 255, was " b)))))

(defn move-into-range
  ([b floor ceiling]
   (let [[floor ceiling] (if (unsigned<= floor ceiling) [floor ceiling] [ceiling floor])
         unchecked-floor (unsign floor)
         range (unsigned-range floor ceiling)]
     (cond
       (and (unsigned<= floor b)                            ;If the value is within the range,
            (unsigned<= b ceiling)) b                       ;just pass it through unchanged.
       (zero? range) floor
       :default (-> (unsign b)
                    (mod (inc range))
                    (+ unchecked-floor)
                    (checked-byte))))))

(defn is-in-range [value range]
  (and (unsigned<= (first range) value)
       (unsigned<= value (last range))))

(defn is-in-ranges [value ranges]
  (->> ranges
       (filter (partial is-in-range value))
       (seq)))

(defn range-by-idx [range]
  (map (fn [lower upper] [lower upper]) (first range) (last range)))

(defn bytes-are-in-range [value range]
  (->> range
       (range-by-idx)
       (map is-in-range value)
       (every? true?)))

(defn pick-range [value ranges]
  (when (seq ranges)
    (nth ranges (mod value (count ranges)))))

(defn value-in-range?
  ([value [floor ceiling]] (value-in-range? value floor ceiling))
  ([value floor ceiling]
   (let [[floor ceiling] (if (unsigned<= ceiling floor)
                           [ceiling floor]
                           [floor ceiling])]
     (and (unsigned<= floor value)
          (unsigned<= value ceiling)))))

(defn values-in-range? [values range]
  (->> (map-indexed #(value-in-range? %2
                                      (nth (first range) %1)
                                      (nth (last range) %1))
                    values)
       (every? true?)))

(defn slice-range [idx range]
  [(nth (first range) idx) (nth (last range) idx)])

(defn slice-ranges [idx ranges]
  (->> ranges
       (map (partial map #(nth %1 idx)))
       (map vec)))

(defn fill [coll target-size value]
  (let [add (- target-size (count coll))]
    (concat coll (repeat add value))))

(defn collapse-identical-ranges [ranges]
  (filter (comp (partial reduce #(and %1 %2) true) #(apply map unsigned<= %)) ranges))

(defn handle-underflow [bytes update-fn]
  (loop [idx (dec (count bytes))
         bytes (vec bytes)]
    (if (<= 0 idx)
      (let [value (get bytes idx)
            updated (update-fn value)]
        (cond
          (and (= 0 value) (= -1 updated)) (recur (dec idx)
                                                  (update bytes idx (constantly -1)))
          (and (= -1 value) (= 0 updated)) (recur (dec idx)
                                                  (update bytes idx (constantly 0)))
          (< updated -128) []
          (< 127 updated) []
          :default (update bytes idx update-fn)))
      [])))

(defn punch-skip-value-out-of-range [range skip-value]
  (let [size-of-range (count (first range))
        fill-lower #(fill %1 size-of-range -1)
        fill-upper (if (negative-range? range)
                     #(fill %1 size-of-range -128)
                     #(fill %1 size-of-range 0))
        lower-range (some-> skip-value
                            (handle-underflow dec)
                            (seq)
                            (fill-lower)
                            (vec)
                            (->> (vector (first range))))
        upper-range (some-> skip-value
                            (handle-underflow inc)
                            (seq)
                            (fill-upper)
                            (vec)
                            (vector (last range)))]
    (->> [lower-range upper-range]
         (filter (comp not nil?))
         (collapse-identical-ranges)
         (vec))))

(defn punch-skip-value-out-if-in-range [range skip-value]
  (if (bytes-are-in-range skip-value range)
    (punch-skip-value-out-of-range range skip-value)
    [range]))

(defn punch-skip-values-out-of-ranges
  [skip-bytes ranges]
  (loop [ranges ranges
         skip-bytes skip-bytes]
    (if-let [skip (first skip-bytes)]
      (recur (mapcat #(punch-skip-value-out-if-in-range %1 skip) ranges)
             (rest skip-bytes))
      (vec ranges))))

(defn map-into-ranges! [^ByteBuffer input ranges]
  (let [limit (.limit input)
        offset (.position input)]
    (loop [idx 0
           ranges ranges
           all-mins true
           all-maxes true]
      (when (and (< (+ offset idx) limit) (seq ranges))     ;Short circuit if we've gone outside the ranges
        (let [input-val (.get input (int (+ offset idx)))   ;This means that we've left the all-mins/all-maxes path
              sliced-ranges (slice-ranges idx ranges)
              range (or (last (is-in-ranges input-val sliced-ranges))
                        (pick-range input-val sliced-ranges))
              floor (if all-mins (first range) 0)
              ceiling (if all-maxes (last range) -1)        ;TODO: some kind of short circuit based on all-mins and all-maxes?
              next-val (move-into-range input-val floor ceiling)]
          (.put input (+ offset idx) next-val)
          (recur (inc idx)
                 (filter (comp (partial value-in-range? next-val) (partial slice-range idx)) ranges)
                 (and all-mins (some true? (map #(= next-val (first %1)) sliced-ranges)))
                 (and all-maxes (some true? (map #(= next-val (last %1)) sliced-ranges)))))))
    input))

(defn split-number-line-min-max-into-bytewise-min-max [floor ceiling ->bytes-fn]
  (if (or (= floor ceiling)
          (and (zero? floor) (pos? ceiling))
          (and (pos? floor) (pos? ceiling))
          (and (neg? floor) (neg? ceiling)))
    [[(->bytes-fn floor) (->bytes-fn ceiling)]]
    [[(->bytes-fn floor) (->bytes-fn -1)] [(->bytes-fn 0) (->bytes-fn ceiling)]]))

(defn split-number-line-ranges-into-bytewise-min-max
  ([ranges ->bytes-fn]
   (->> ranges
        (partition 2)
        (mapcat (fn [[floor ceiling]] (split-number-line-min-max-into-bytewise-min-max floor ceiling ->bytes-fn))))))

(defn byte->bytes [b]
  (let [out (byte-array 1)]
    (aset-byte out 0 b)
    out))

(defn bytes->short [b1 b2]
  (-> (bit-or (bit-and 0xff b2)
              (bit-shift-left (bit-and 0xff b1) 8))
      (unchecked-short)))

(defn short->bytes [s]
  (let [out (byte-array 2)
        wrapped (ByteBuffer/wrap out)]
    (.putShort wrapped s)
    out))

(defn bytes->int [b1 b2 b3 b4]
  (-> (bit-or (bit-and 0xff b4)
              (bit-shift-left (bit-and 0xff b3) 8)
              (bit-shift-left (bit-and 0xff b2) 16)
              (bit-shift-left (bit-and 0xff b1) 24))
      (unchecked-int)))

(defn int->bytes [^Integer i]
  (let [out (byte-array 4)
        wrapped (ByteBuffer/wrap out)]
    (.putInt wrapped i)
    out))

(defn bytes->long [b1 b2 b3 b4 b5 b6 b7 b8]
  (-> (bit-or (bit-and 0xff b8)
              (bit-shift-left (bit-and 0xff b7) 8)
              (bit-shift-left (bit-and 0xff b6) 16)
              (bit-shift-left (bit-and 0xff b5) 24)
              (bit-shift-left (bit-and 0xff b4) 32)
              (bit-shift-left (bit-and 0xff b3) 40)
              (bit-shift-left (bit-and 0xff b2) 48)
              (bit-shift-left (bit-and 0xff b1) 56))
      (unchecked-long)))

(defn long->bytes [^Long i]
  (let [out (byte-array 8)
        wrapped (ByteBuffer/wrap out)]
    (.putLong wrapped i)
    out))


(defn bytes->float [b1 b2 b3 b4]
  (-> (bit-or (bit-and 0xff b4)
              (bit-shift-left (bit-and 0xff b3) 8)
              (bit-shift-left (bit-and 0xff b2) 16)
              (bit-shift-left (bit-and 0xff b1) 24))
      (unchecked-int)
      (Float/intBitsToFloat)))

(defn float->bytes [^Float f]
  (let [out (byte-array 4)
        wrapped (ByteBuffer/wrap out)]
    (.putFloat wrapped f)
    out))

(defn bytes->double [b1 b2 b3 b4 b5 b6 b7 b8]
  (-> (bit-or (bit-and 0xff b8)
              (bit-shift-left (bit-and 0xff b7) 8)
              (bit-shift-left (bit-and 0xff b6) 16)
              (bit-shift-left (bit-and 0xff b5) 24)
              (bit-shift-left (bit-and 0xff b4) 32)
              (bit-shift-left (bit-and 0xff b3) 40)
              (bit-shift-left (bit-and 0xff b2) 48)
              (bit-shift-left (bit-and 0xff b1) 56))
      (unchecked-long)
      (Double/longBitsToDouble)))

(defn double->bytes [^Double d]
  (let [out (byte-array 8)
        wrapped (ByteBuffer/wrap out)]
    (.putDouble wrapped d)
    out))

(defn length-of-buffers [^Collection buffers]
  (->> buffers
       (map (fn [^ByteBuffer buf] (.remaining buf)))
       (reduce + 0)))

(defn buffers->bytes
  ([^Collection buffers]
   (->> buffers
        (mapcat #(for [i (range (.position %1) (.limit %1))]
                   (.get %1 i)))
        (byte-array)))
  ([^Collection buffers start end]
   (->> buffers
        (drop start)
        (take (- end start))
        (mapcat #(for [i (range (.position %1) (.limit %1))]
                   (.get %1 i)))
        (byte-array))))
