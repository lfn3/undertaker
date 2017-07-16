(ns undertaker.source.forgetful
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s])
  (:import (java.util Random)))

(defn squish-byte [b floor ceiling]
  (let [b (bit-and 0xff b)
        [floor ceiling] [(min floor ceiling) (max floor ceiling)]
        range (- ceiling floor)]
    (unchecked-byte (cond
                      (= ceiling floor) ceiling
                      (and (< b ceiling) (>= b floor)) b
                      :default (+ floor (mod b range))))))

(extend-type Random
  proto/ByteSource
  (get-byte [this min max]                                  ;result will be [min, max)
    (let [output (byte-array 1)]
      (.nextBytes this output)
      (squish-byte (aget output 0) min max))))

(defrecord ForgetfulSource
  [rnd]
  proto/ByteSource
  (get-byte [_ min max] (proto/get-byte rnd min max))
  proto/Interval
  (push-interval [_ interval-name])
  (pop-interval [_ interval-id generated-value])
  (get-intervals [_] [])
  proto/Recall
  (get-sourced-bytes [_] (byte-array 1))
  (reset [_]))

(defn make-source [seed]
  (let [rnd (Random. seed)]
    (->ForgetfulSource rnd)))

(s/fdef make-source
  :args (s/cat :seed integer?)
  :ret (comp (partial extends? proto/ByteSource) class))
