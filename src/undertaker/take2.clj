(ns undertaker.take2
  (:require [undertaker.proto :as proto])
  (:import (java.util Random)))

(defn get-bytes [b number]
  (byte-array (repeatedly number (proto/get-byte b))))

(extend-type Random
  proto/ByteSource
  (get-byte [this] (.nextBytes this (byte-array 1))))

(defn take-byte
  ([source] (take-byte source Byte/MIN_VALUE Byte/MAX_VALUE))
  ([source ^Byte max] (take-byte source Byte/MIN_VALUE max))
  ([source ^Byte min ^Byte max]
   (let [range (- max min)]
     (proto/get-byte source))))

(defn take-bytes
  ([source ^Byte min ^Byte max number]
    (get-bytes source number)))
