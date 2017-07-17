(ns undertaker.source
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [undertaker.source.wrapped-random]
            [undertaker.util :as util])
  (:import (java.util Random)))

(s/def ::source (s/with-gen (comp (partial extends? proto/ByteSource) class)
                            #(s.gen/fmap undertaker.source.wrapped-random/make-source (s.gen/int))))

(defn get-byte
  ([source] (proto/get-byte source Byte/MIN_VALUE Byte/MAX_VALUE))
  ([source min] (proto/get-byte source min Byte/MAX_VALUE))
  ([source min max] (proto/get-byte source min max)))

(s/fdef get-byte
  :args (s/cat :source ::source :min (s/? ::util/byte) :max (s/? ::util/byte))
  :ret (s/and ::util/byte
              (partial >= Byte/MAX_VALUE)
              (partial <= Byte/MIN_VALUE)))

(s/fdef get-byte
        :args (s/cat :source ::source
                     :min (s/? (s/and integer? (partial <= 0)))
                     :max (s/? (s/and integer? (partial >= 256))))
        :ret integer?)

(defn push-interval [source interval-name] (proto/push-interval source interval-name))
(defn pop-interval [source interval-id generated-value] (proto/pop-interval source interval-id generated-value))
(defn get-intervals [source] (proto/get-intervals source))

(defn get-sourced-bytes [source] (proto/get-sourced-bytes source))
(defn reset [source] (proto/reset source))

