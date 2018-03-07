(ns net.lfn3.undertaker.specs.bytes
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [net.lfn3.undertaker.bytes :as bytes]
            [clojure.test.check.generators :as gen])
  (:import (java.nio ByteBuffer)))

(s/fdef bytes/byte?
  :args (s/cat :b number?)
  :ret boolean?)

(s/fdef bytes/range<
  :args (s/cat :range1 ::bytes/range :range2 ::bytes/range)
  :ret boolean?)

(s/def ::bytes/byte (s/with-gen
                      (s/and integer?
                             bytes/byte?)
                      #(s/gen (set (range Byte/MIN_VALUE Byte/MAX_VALUE)))))

(s/def ::bytes/bytes (s/or :arr bytes?
                           :coll (s/coll-of ::bytes/byte)))

(s/def ::bytes/bytes-to-skip (s/with-gen (s/and (s/coll-of ::bytes/bytes) set?)
                                         #(s.gen/fmap set (s/gen (s/coll-of ::bytes/bytes)))))
(s/def ::bytes/range (s/with-gen (s/and (s/tuple ::bytes/bytes ::bytes/bytes)
                                        (comp bytes/conformed-ranges-have-same-length? vector))
                                 #(gen/bind gen/nat
                                            (fn [size]
                                              (let [g (gen/vector gen/byte size)]
                                                (gen/tuple g g))))))

(defn range-gen
  ([] (gen/bind gen/nat range-gen))
  ([size] (let [g (gen/vector gen/byte size)]
            (gen/vector (gen/tuple g g)))))

(s/def ::bytes/ranges (s/with-gen (s/and (s/coll-of ::bytes/range)
                                         bytes/ranges-are-sorted?
                                         bytes/conformed-ranges-have-same-length?)
                                  range-gen))
(s/def ::bytes/sliced-range (s/tuple ::bytes/byte ::bytes/byte))
(s/def ::bytes/sliced-ranges (s/coll-of ::bytes/sliced-range))
(s/def ::bytes/byte-buffer (s/with-gen (partial instance? ByteBuffer)
                                       (fn [] (gen/fmap #(ByteBuffer/wrap %1) gen/bytes))))
(s/def ::bytes/byte-buffers (s/coll-of ::bytes/byte-buffer))

(s/fdef bytes/unsigned<=
  :args (s/cat :x int? :y int?)
  :ret boolean?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [x y]} args]
          (if (= (Long/signum x) (Long/signum y))
            (= ret (<= x y))
            (= ret (or (zero? x) (neg? y)))))))

(s/fdef bytes/move-into-range
  :args (s/cat :b ::bytes/byte :floor ::bytes/byte :ceiling ::bytes/byte)
  :ret ::bytes/byte)

(s/fdef bytes/is-in-range
  :args (s/cat :value ::bytes/byte :range ::bytes/sliced-range)
  :ret boolean?)

(s/fdef bytes/is-in-ranges
  :args (s/cat :value ::bytes/byte :ranges ::bytes/sliced-ranges)
  :ret (s/nilable ::bytes/sliced-ranges))

(s/fdef bytes/bytes-are-in-range
  :args (s/and (s/cat :value ::bytes/bytes :range ::bytes/range)
               (fn [{:keys [value range]}]
                 (<= (count (val value)) (count (val (first range))))))
  :ret boolean?)

(s/fdef bytes/pick-range
  :args (s/cat :value ::bytes/byte :ranges ::bytes/sliced-ranges)
  :ret (s/nilable ::bytes/sliced-range))

(s/fdef bytes/value-in-range?
  :args (s/or :sliced-range (s/cat :value ::bytes/byte :sliced-range ::bytes/sliced-range)
              :separate-vals (s/cat :value ::bytes/byte :floor ::bytes/byte :ceiling ::bytes/byte))
  :ret boolean?)

(defn conformed-range-length [range]
  (-> range
      (first)
      (last)
      (count)))

(s/fdef bytes/slice-range
  :args (s/and (s/cat :idx nat-int? :range ::bytes/range)
               (fn [{:keys [idx range]}]
                 (< idx (conformed-range-length range))))
  :ret ::bytes/sliced-range)

(s/fdef bytes/slice-ranges
  :args (s/and (s/cat :idx nat-int? :ranges ::bytes/ranges)
               (fn [{:keys [idx ranges]}]
                 (< idx (conformed-range-length (first ranges)))))
  :ret ::bytes/sliced-ranges)

(s/fdef bytes/collapse-identical-ranges
  :args (s/cat :ranges ::bytes/ranges)
  :ret ::bytes/ranges)

(s/fdef bytes/handle-underflow
  :args (s/cat :bytes ::bytes/bytes :update-fn (s/with-gen fn? #(gen/return inc)))
  :ret ::bytes/bytes)

(defn skip-val-must-be-equal-or-shorter-length-than-range
  ([{:keys [range skip-value]}]
   (skip-val-must-be-equal-or-shorter-length-than-range range skip-value))
  ([range skip-value]
   (let [range-len (count (val (first range)))
         skip-val-len (count (some-> skip-value
                                     (val)))]
     (<= skip-val-len range-len))))

(s/fdef bytes/punch-skip-value-out-of-range
  :args (s/and (s/cat :range ::bytes/range :skip-value ::bytes/bytes)
               skip-val-must-be-equal-or-shorter-length-than-range)
  :ret ::bytes/ranges)

(s/fdef bytes/punch-skip-value-out-if-in-range
  :args (s/and (s/cat :range ::bytes/range :skip-value ::bytes/bytes)
               skip-val-must-be-equal-or-shorter-length-than-range)
  :ret ::bytes/ranges)

(s/fdef bytes/punch-skip-values-out-of-ranges
  :args (s/and (s/cat :skip (s/coll-of ::bytes/bytes) :ranges ::bytes/ranges)
               (fn [{:keys [skip ranges]}]
                 (every? true?
                         (for [skip skip
                               range ranges]
                           (skip-val-must-be-equal-or-shorter-length-than-range range skip)))))
  :ret ::bytes/ranges)

(s/fdef bytes/map-into-ranges!
  :args (s/with-gen (s/and (s/cat :input ::bytes/byte-buffer :ranges ::bytes/ranges)
                           (fn [{:keys [input ranges]}]
                             (= (.remaining input) (conformed-range-length (first ranges)))))
                    #(gen/bind (s/gen ::bytes/byte-buffer)
                               (fn [bb] (gen/tuple (gen/return bb)
                                                   (range-gen (.limit bb))))))
  :ret (partial instance? ByteBuffer))

(s/def ::bytes/byte-buffer (s/with-gen (partial instance? ByteBuffer)
                                       #(gen/fmap (fn [^bytes b] (ByteBuffer/wrap b)) gen/bytes)))

(s/def ::bytes/->bytes-fn (s/with-gen fn? #_(s/fspec :args (s/cat :number number?) :ret bytes?)
                                      #(gen/return (fn [n] (case (str (class n))
                                                             "class java.lang.Long" (bytes/long->bytes n)
                                                             "class java.lang.Double" (bytes/double->bytes n))))))

(s/fdef bytes/split-number-line-min-max-into-bytewise-min-max
  :args (s/cat :floor number? :ceiling number? :->bytes-fn ::bytes/->bytes-fn)
  :ret ::bytes/ranges)

(s/fdef bytes/split-number-line-ranges-into-bytewise-min-max
  :args (s/cat :ranges (s/and (s/coll-of number?) (comp even? count))
               :->bytes-fn ::bytes/->bytes-fn)
  :ret ::bytes/ranges)

(s/def ::bytes/short (s/and int?
                            #(<= Short/MIN_VALUE %1)
                            #(<= %1 Short/MAX_VALUE)))

(s/fdef bytes/short->bytes
  :args (s/cat :s ::bytes/short)
  :ret ::bytes/bytes)

(s/def ::bytes/int (s/and int?
                          #(<= Integer/MIN_VALUE %1)
                          #(<= %1 Integer/MAX_VALUE)))

(s/fdef bytes/int->bytes
  :args (s/cat :i ::bytes/int)
  :ret bytes?)

(s/fdef bytes/bytes->short
  :args (s/cat :b1 ::bytes/byte :b2 ::bytes/byte)
  :ret ::bytes/short
  :fn (fn [{:keys [args ret]}]
        (let [arr (byte-array (vals args))]
          (and (= ret (.getShort (ByteBuffer/wrap arr)))
               (= (vec (bytes/short->bytes ret)) (vec arr))))))

(s/fdef bytes/bytes->int
  :args (s/cat :b1 ::bytes/byte :b2 ::bytes/byte :b3 ::bytes/byte :b4 ::bytes/byte)
  :ret ::bytes/int
  :fn (fn [{:keys [args ret]}]
        (let [arr (byte-array (vals args))]
          (and (= ret (.getInt (ByteBuffer/wrap arr)))
               (= (vec (bytes/int->bytes ret)) (vec arr))))))

(s/fdef bytes/bytes->long
  :args (s/cat :b1 ::bytes/byte
               :b2 ::bytes/byte
               :b3 ::bytes/byte
               :b4 ::bytes/byte
               :b5 ::bytes/byte
               :b6 ::bytes/byte
               :b7 ::bytes/byte
               :b8 ::bytes/byte)
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (let [arr (byte-array (vals args))]
          (and (= ret (.getLong (ByteBuffer/wrap arr)))
               (= (vec (bytes/long->bytes ret)) (vec arr))))))

(defn equal-or-both-nan? [v1 v2]
  (or (= v1 v2)
      (and (float? v1) (Float/isNaN v1) (float? v2) (Float/isNaN v2))
      (and (double? v1) (Double/isNaN v1) (double? v2) (Double/isNaN v2))))

(s/fdef bytes/bytes->float
  :args (s/cat :b1 ::bytes/byte :b2 ::bytes/byte :b3 ::bytes/byte :b4 ::bytes/byte)
  :ret number?
  :fn (fn [{:keys [args ret]}]
        (let [arr (byte-array (vals args))]
          (and (equal-or-both-nan? ret (.getFloat (ByteBuffer/wrap arr)))
               (= (vec (bytes/float->bytes ret)) (vec arr))))))

(s/fdef bytes/bytes->double
  :args (s/cat :b1 ::bytes/byte
               :b2 ::bytes/byte
               :b3 ::bytes/byte
               :b4 ::bytes/byte
               :b5 ::bytes/byte
               :b6 ::bytes/byte
               :b7 ::bytes/byte
               :b8 ::bytes/byte)
  :ret number?
  :fn (fn [{:keys [args ret]}]
        (let [arr (byte-array (vals args))]
          (and (equal-or-both-nan? ret (.getDouble (ByteBuffer/wrap arr)))
               (= (vec (bytes/double->bytes ret)) (vec arr))))))