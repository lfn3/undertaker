(ns undertaker.source
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [undertaker.source.wrapped-random])
  (:import (java.util Random)))

(s/def ::source (s/with-gen (comp (partial extends? proto/ByteSource) class)
                            #(s.gen/fmap undertaker.source.wrapped-random/make-source (s.gen/int))))

(defn get-byte ([source] (proto/get-byte source)))

(s/fdef get-byte
  :args (s/cat :source ::source)
  :ret (s/and integer?
              (partial >= Byte/MAX_VALUE)
              (partial <= Byte/MIN_VALUE)))

(defn get-bytes ([source number] (proto/get-bytes source number)))

(s/fdef get-bytes
  :args (s/cat :source ::source :number integer?)
  :ret bytes?)

(defn push-interval [source interval-name] (proto/push-interval source interval-name))
(defn pop-interval [source interval-id generated-value] (proto/pop-interval source interval-id generated-value))
(defn get-intervals [source] (proto/get-intervals source))
(defn get-sourced-bytes [source] (proto/get-sourced-bytes source))
