(ns undertaker.util
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set])
  (:import (java.nio ByteBuffer)))

(def bug-tracker-url "https://github.com/lfn3/undertaker/issues/new")

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

(defn get-bytes-from-int [^Integer i]
  (let [out (byte-array 4)
        wrapped (ByteBuffer/wrap out)]
    (.putInt wrapped i)
    out))

(s/fdef get-bytes-from-int
  :args (s/cat :i integer?)
  :ret bytes?)

(defn get-bytes-from-long [^Long i]
  (let [out (byte-array 8)
        wrapped (ByteBuffer/wrap out)]
    (.putLong wrapped i)
    out))

(defn get-bytes-from-double [^Double d]
  (let [out (byte-array 8)
        wrapped (ByteBuffer/wrap out)]
    (.putDouble wrapped d)
    out))

(defn byte-array->bits [bytes]
  (map #(.substring (Integer/toBinaryString (+ (bit-and 0xFF %1) 0x100)) 1) bytes))

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

(defn signed-range->unsigned [min max]
  (unchecked-byte (- max min)))

(s/fdef signed-range->unsigned
  :args (s/cat :min ::byte :max ::byte)
  :ret ::byte
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]} args]
          (let [range (if (< min max)
                        (range min max)
                        (range min max -1))]
            (count range)))))

(defn map-unsigned-byte-into-signed-range
  "This is somewhat odd since I still want zero to map to zero,
   or to the \"simplest\" value, i.e. value closest to zero (sml).
   What I do is set 0 = sml, and wrap around max back to min,
   unless the range is entirely negative in which case we run from max down to min"
  [min max value]
  (let [range-is-entirely-negative (and (neg? max) (neg? min))
        range-is-entirely-positive (and (pos? max) (pos? min))
        sml (cond
              range-is-entirely-negative max
              range-is-entirely-positive min
              :default 0)]
    (cond
      range-is-entirely-negative (-> value                  ;in this case sml is the number closest to -1
                                     (bit-and 0xff)         ;move value into the range from 0 - 255
                                     (-)                    ;negate the value so we move more towards -128
                                     (+ sml))
      range-is-entirely-positive (-> value
                                     (bit-and 0xff)
                                     (+ sml))
      :default (let [possibly-wrapped-val (-> value
                                              (bit-and 0xff)
                                              (+ sml))]
                 (if (< max possibly-wrapped-val)
                   (-> possibly-wrapped-val                 ;If wrapped, treat the same as the -ve case above.
                       (- max)                              ;But we have to remove the quantity from before wrapping first.
                       (-))                                 ;Sml will be zero, no need to add.
                   value)))))

(s/fdef map-unsigned-byte-into-signed-range
  :args (s/cat :min ::byte :max ::byte :value ::byte)
  :ret ::byte
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max value]} args]
          (and (<= min ret)
               (<= ret max)))))

(defn map-unsigned-byte-into-unsigned-range
  "Same reasoning as map-unsigned-byte-into-signed-range,
   but values go from -128 -> min on the negative side."
  [min max value]
  (if (unsigned<= value max)
    value
    (-> (- (bit-and 0xff value) (bit-and 0xff max))
        (+ -128))))
