(ns net.lfn3.undertaker.core-unbound
  (:refer-clojure :exclude [int byte long double short char float keyword boolean shuffle symbol list])
  (:require [net.lfn3.undertaker.source :as source]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.proto :as proto]
            [clojure.core :as core])
  (:import (java.nio ByteBuffer)
           (net.lfn3.undertaker UniqueInputValuesExhaustedException)
           (java.util List)
           (java.lang.reflect Method Modifier)))

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

(defmacro numeric [source ranges min-neg-val ->bytes-fn byte-buffer->]
  `(with-leaf-interval ~source
     (->> (bytes/split-number-line-ranges-into-bytewise-min-max ~ranges ~min-neg-val ~->bytes-fn)
          (source/get-bytes ~source)
          (get-from-byte-buffer-abs ~byte-buffer->))))

(defn byte [source & ranges] (numeric source ranges -1 bytes/byte->bytes .get))

(defn boolean [source] (with-leaf-interval source (= 1 (byte source 0 1))))

(defn short [source & ranges] (numeric source ranges -1 bytes/short->bytes .getShort))

(defn int [source & ranges] (numeric source ranges -1 bytes/int->bytes .getInt))

(defn vector-ranges-to-byte-ranges [ranges]
  (vec (map (comp vec (partial map byte-array)) ranges)))

(def default-char-range (vector-ranges-to-byte-ranges [[[0x0000] [0xD800]]]))
(def ascii-range (vector-ranges-to-byte-ranges [[[32] [126]]]))
(def alphanumeric-range (vector-ranges-to-byte-ranges [[[48] [57]]
                                                       [[65] [90]]
                                                       [[97] [122]]]))
(def alpha-range (vector-ranges-to-byte-ranges [[[65] [90]]
                                                [[97] [122]]]))

(defn char [source ranges]
  (with-leaf-interval source
    (->> ranges
         (source/get-bytes source)
         (get-from-byte-buffer-abs .get)
         (core/char))))

(defn long [source & ranges] (numeric source ranges -1 bytes/long->bytes .getLong))

(defn float [source & ranges] (numeric source ranges (- Float/MIN_VALUE) bytes/float->bytes .getFloat))

(def start-of-unreal-doubles (->> (range -1 -17 -1)
                                  (mapcat (fn [i] [[127 i] [-1 i]]))
                                  (set)))

(defn real-double [source & ranges]
  (with-leaf-interval source
    (->> (bytes/split-number-line-ranges-into-bytewise-min-max ranges (- Double/MIN_VALUE) bytes/double->bytes)
         ;This is the reason we don't use the `numeric` macro
         (bytes/punch-skip-values-out-of-ranges start-of-unreal-doubles)
         (source/get-bytes source)
         (get-from-byte-buffer-abs .getDouble))))

(defn double [source & ranges] (numeric source ranges (- Double/MIN_VALUE) bytes/double->bytes .getDouble))

;TODO bias this so it's more likely to produce longer seqs.
(defn should-generate-elem? [source floor ceiling len]
  (with-leaf-interval source
    (<= 1 (let [value (byte source 0 5)]                            ;Side-effecty
            (cond (< len floor) 1
                  (< ceiling (inc len)) 0
                  :default value)))))

(def default-collection-max-size 64)
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

(def default-string-max-size 2048)
(defn string
  [source char-range min max]
  (with-compound-interval source
    (-> (collection source vector (partial char source char-range) conj min max)
        (char-array)
        (String.))))

(defn default-class->generator [source] {String #(string source default-char-range 0 default-string-max-size)
                                         Integer/TYPE #(int source Integer/MIN_VALUE Integer/MAX_VALUE)
                                         Integer #(int source Integer/MIN_VALUE Integer/MAX_VALUE)
                                         Long #(long source Long/MIN_VALUE Long/MAX_VALUE)
                                         Long/TYPE #(long source Long/MIN_VALUE Long/MAX_VALUE)
                                         Float #(float source (- Float/MAX_VALUE) Float/MAX_VALUE)
                                         Float/TYPE #(float source (- Float/MAX_VALUE) Float/MAX_VALUE)
                                         Double #(double source (- Double/MAX_VALUE) Double/MAX_VALUE)
                                         Double/TYPE #(double source (- Double/MAX_VALUE) Double/MAX_VALUE)
                                         Character #(char source default-char-range)
                                         Character/TYPE #(char source default-char-range)
                                         Byte #(byte source Byte/MIN_VALUE Byte/MAX_VALUE)
                                         Byte/TYPE #(byte source Byte/MIN_VALUE Byte/MAX_VALUE)
                                         Boolean #(boolean source)
                                         Boolean/TYPE #(boolean source)})

(defn is-static-constructor [^Class c ^Method m]
  (and (-> m (.getModifiers) (Modifier/isStatic))
       (-> m (.getReturnType) (= c))))

(defn get-static-constructors [^Class c]
  (->> (.getDeclaredMethods c)
       (filter (partial is-static-constructor c))))

(defn find-type-params [class constructor]
  )

(def reflective*)

(defn generic-class->generator [source]
  {List (fn [[type]]
          (let [ctor nil                                    ;FIXME
                ]
            (collection source
                        vector
                        (partial reflective* source type (find-type-params class ctor))
                        conj
                        0
                        default-collection-max-size)))})

(defn reflective* [source class class->generator type-param->class]
  (or (some-> class->generator
              (seq)
              (get class)
              (apply nil))))

(defn reflective
  [source class class->generator]
  (reflective* source
               class
               (merge (default-class->generator source) class->generator)
               {}))