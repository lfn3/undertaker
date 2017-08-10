(ns undertaker.util
  (:require [clojure.spec.alpha :as s])
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
  (= -1 (Long/compareUnsigned x y)))

(s/fdef unsigned<=
  :args (s/cat :x int? :y int?)
  :ret boolean?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [x y]} args]
          (if (= (Long/signum x) (Long/signum y))
            (and ret (<= x y))
            (and ret (neg? y))))))
