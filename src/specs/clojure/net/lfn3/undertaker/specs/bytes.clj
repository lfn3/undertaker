(ns net.lfn3.undertaker.specs.bytes
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [net.lfn3.undertaker.bytes :as bytes])
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
(s/def ::bytes/range (s/tuple ::bytes/bytes ::bytes/bytes))
(s/def ::bytes/ranges (s/and (s/coll-of ::bytes/range)
                       bytes/ranges-are-sorted?))
(s/def ::bytes/sliced-range (s/tuple ::bytes/byte ::bytes/byte))
(s/def ::bytes/sliced-ranges (s/coll-of ::bytes/sliced-range))

(s/fdef bytes/unsigned<=
  :args (s/cat :x int? :y int?)
  :ret boolean?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [x y]} args]
          (if (= (Long/signum x) (Long/signum y))
            (= ret (<= x y))
            (= ret (or (zero? x) (neg? y)))))))

(s/fdef bytes/move-into-range
  :args (s/cat :b ::bytes/byte :floor ::bytes/byte :ceiling ::bytes/byte :skip-values (s/? ::bytes/bytes))
  :ret ::bytes/byte)

(s/fdef bytes/is-in-range
  :args (s/cat :value ::bytes/byte :range ::bytes/sliced-range)
  :ret boolean?)

(s/fdef bytes/is-in-ranges
  :args (s/cat :value ::bytes/byte :ranges ::bytes/sliced-ranges)
  :ret (s/nilable ::bytes/sliced-ranges))

(s/fdef bytes/bytes-are-in-range
  :args (s/cat :value ::bytes/bytes :range ::bytes/range)
  :ret boolean?)

(s/fdef bytes/pick-range
  :args (s/cat :value ::bytes/byte :ranges ::bytes/sliced-ranges)
  :ret (s/nilable ::bytes/sliced-range))

(s/fdef bytes/value-in-range?
  :args (s/or :sliced-range (s/cat :value ::bytes/byte :sliced-range ::bytes/sliced-range)
              :separate-vals (s/cat :value ::bytes/byte :floor ::bytes/byte :ceiling ::bytes/byte))
  :ret boolean?)

(s/fdef bytes/slice-range
  :args (s/cat :idx int? :range ::bytes/range)
  :ret ::bytes/sliced-range)

(s/fdef bytes/slice-ranges
  :args (s/cat :idx int? :ranges ::bytes/ranges)
  :ret ::bytes/sliced-ranges)

(s/fdef bytes/collapse-identical-ranges
  :args (s/cat :ranges ::bytes/ranges)
  :ret ::bytes/ranges)

(s/fdef bytes/punch-skip-value-out-of-range
  :args (s/cat :range ::bytes/range :skip-value ::bytes/bytes)
  :ret ::bytes/ranges)

(s/fdef bytes/punch-skip-value-out-if-in-range
  :args (s/cat :range ::bytes/range :skip ::bytes/bytes)
  :ret ::bytes/ranges)

(s/fdef bytes/punch-skip-values-out-of-ranges
  :args (s/cat :skip (s/coll-of ::bytes/bytes) :ranges ::bytes/ranges)
  :ret ::bytes/ranges)

(s/fdef bytes/map-into-ranges!
  :args (s/cat :input (partial instance? ByteBuffer) :ranges ::bytes/ranges :skip-values (s/? ::bytes/bytes-to-skip))
  :ret (partial instance? ByteBuffer)
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [input ranges skip-values]} args]
          (if-not (nil? skip-values)
            (not (skip-values (.array ret)))
            true))))

(s/fdef bytes/split-number-line-min-max-into-bytewise-min-max
  :args (s/cat :floor number? :ceiling number? :->bytes-fn fn?)
  :ret ::bytes/ranges)

(s/fdef bytes/split-number-line-ranges-into-bytewise-min-max
  :args (s/cat :ranges (s/and (s/coll-of number?) (comp even? count))
               :->bytes-fn fn?)
  :ret ::bytes/ranges)

(s/def ::bytes/short (s/and int?
                      #(<= Short/MIN_VALUE %1)
                      #(<= %1 Short/MAX_VALUE)))

(s/fdef bytes/short->bytes
  :args (s/cat :s ::bytes/short)
  :ret ::bytes/bytes)

(s/fdef bytes/int->bytes
  :args (s/cat :i integer?)
  :ret bytes?)
