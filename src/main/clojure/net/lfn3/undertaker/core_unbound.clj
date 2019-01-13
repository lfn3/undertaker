(ns net.lfn3.undertaker.core-unbound
  (:refer-clojure :exclude [int byte long double short char float keyword boolean shuffle symbol list])
  (:require [net.lfn3.undertaker.source :as source]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.proto :as proto]
            [clojure.core :as core])
  (:import (java.nio ByteBuffer)
           (net.lfn3.undertaker UniqueInputValuesExhaustedException)))

(defmacro with-interval-and-hints [source type hints & body]
  `(do
     (source/push-interval ~source ~type ~hints)
     (let [result# (do ~@body)]
       (source/pop-interval ~source result#)
       result#)))

(defmacro with-leaf-interval-and-hints [source hints & body]
  `(with-interval-and-hints ~source ::proto/leaf-interval ~hints ~@body))

(defmacro with-leaf-interval [source & body]
  `(with-leaf-interval-and-hints ~source #{} ~@body))

(defmacro with-compound-interval-and-hints [source hints & body]
  `(with-interval-and-hints ~source ::proto/compound-interval ~hints ~@body))

(defmacro with-compound-interval [source & body]
  `(with-compound-interval-and-hints ~source #{} ~@body))

(defmacro get-from-byte-buffer-abs
  "Get from a byte-buffer without advancing it's position."
  [f ^ByteBuffer byte-buffer]
  `(let [buffer# ~byte-buffer]
     (~f buffer# (.position buffer#))))

(defn byte [source min max]
  (with-leaf-interval source                                ;This is slightly ridiculous, but consistency is key!
    (->> (bytes/split-number-line-ranges-into-bytewise-min-max [min max] bytes/byte->bytes)
         (source/get-bytes source)
         (get-from-byte-buffer-abs .get))))

(defn boolean [source] (with-leaf-interval source (= 1 (byte source 0 1))))

(defn short [source floor ceiling & more-ranges]
  (with-leaf-interval source
    (->> (bytes/split-number-line-ranges-into-bytewise-min-max (concat [floor ceiling] more-ranges) bytes/short->bytes)
         (source/get-bytes source)
         (get-from-byte-buffer-abs .getShort))))

(defn int [source floor ceiling & more-ranges]
  (with-leaf-interval source
    (->> (bytes/split-number-line-ranges-into-bytewise-min-max (concat [floor ceiling] more-ranges) bytes/int->bytes)
         (source/get-bytes source)
         (get-from-byte-buffer-abs .getInt))))

(defn char [source ranges]
  (with-leaf-interval source
    (->> ranges
         (source/get-bytes source)
         (get-from-byte-buffer-abs .get)
         (core/char))))

(defn long [source floor ceiling & more-ranges]
  (with-leaf-interval source
    (->> (bytes/split-number-line-ranges-into-bytewise-min-max
           (concat [floor ceiling] more-ranges) bytes/long->bytes)
         (source/get-bytes source)
         (get-from-byte-buffer-abs .getLong))))

(defn float [source floor ceiling & more-ranges]
  (with-leaf-interval source
    (->> (bytes/split-number-line-ranges-into-bytewise-min-max
           (concat [floor ceiling] more-ranges) (- Float/MIN_VALUE) bytes/float->bytes)
         (source/get-bytes source)
         (get-from-byte-buffer-abs .getFloat))))

(def start-of-unreal-doubles (->> (range -1 -17 -1)
                                  (mapcat (fn [i] [[127 i] [-1 i]]))
                                  (set)))

(defn real-double [source floor ceiling & more-ranges]
  (with-leaf-interval source
    (->> (bytes/split-number-line-ranges-into-bytewise-min-max
           (concat [floor ceiling] more-ranges) (- Double/MIN_VALUE) bytes/double->bytes)
         (bytes/punch-skip-values-out-of-ranges start-of-unreal-doubles)
         (source/get-bytes source)
         (get-from-byte-buffer-abs .getDouble))))

(defn double [source floor ceiling & more-ranges]
  (with-leaf-interval source
    (->> (bytes/split-number-line-ranges-into-bytewise-min-max
           (concat [floor ceiling] more-ranges) (- Double/MIN_VALUE) bytes/double->bytes)
         (source/get-bytes source)
         (get-from-byte-buffer-abs .getDouble))))


;TODO bias this so it's more likely to produce longer seqs.
(defn should-generate-elem? [source floor ceiling len]
  (with-leaf-interval source
    (<= 1 (let [value (byte source 0 5)]                            ;Side-effecty
            (cond (< len floor) 1
                  (< ceiling (inc len)) 0
                  :default value)))))

(defmacro collection
  "Because generation-fn is user supplied, it has to dynamically resolve source"
  ([source collection-init-fn generation-fn add-to-coll-fn min-size max-size]
   `(with-compound-interval ~source
      (loop [result# (~collection-init-fn)]
        (let [next# (with-compound-interval-and-hints ~source [[::proto/snippable nil]]
                      (if (should-generate-elem? ~source ~min-size ~max-size (count result#))
                        (try
                          (~generation-fn)
                          (catch UniqueInputValuesExhaustedException e#
                            (if (and (< ~min-size (count result#))
                                     (< (count result#) ~max-size))
                              ::stop-collection-generation
                              (throw e#))))
                        ::stop-collection-generation))]
          (if-not (= ::stop-collection-generation next#)
            (recur (~add-to-coll-fn result# next#))
            result#))))))

(defn string
  [source char-range min max]
  (with-compound-interval source
    (-> (collection source vector (partial char source char-range) conj min max)
        (char-array)
        (String.))))
