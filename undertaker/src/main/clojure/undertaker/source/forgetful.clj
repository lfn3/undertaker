(ns undertaker.source.forgetful
  (:require [undertaker.proto :as proto]
            [clojure.spec.alpha :as s]
            [undertaker.util :as util])
  (:import (java.util Random)))

(defn squish-byte [b floor ceiling]
  (let [range (inc (util/abs (- ceiling floor)))]
    (unchecked-byte (+ floor (mod b range)))))

(extend-type Random
  proto/ByteSource
  (get-byte [this min max]
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
