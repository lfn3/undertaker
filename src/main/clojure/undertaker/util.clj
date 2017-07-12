(ns undertaker.util
  (:require [clojure.spec.alpha :as s])
  (:import (java.nio ByteBuffer)))

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