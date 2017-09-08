(ns com.lmax.undertaker.junit.source-rule
  (:gen-class
    :name com.lmax.undertaker.junit.SourceRule
    :state state
    :implements [org.junit.rules.TestRule
                 com.lmax.undertaker.junit.generators.ByteGen
                 com.lmax.undertaker.junit.generators.IntGen
                 com.lmax.undertaker.junit.generators.LongGen
                 com.lmax.undertaker.junit.generators.CharGen
                 com.lmax.undertaker.junit.generators.DoubleGen
                 com.lmax.undertaker.junit.generators.FloatGen
                 com.lmax.undertaker.junit.generators.ShortGen
                 com.lmax.undertaker.junit.generators.BoolGen
                 com.lmax.undertaker.junit.generators.ListGen
                 com.lmax.undertaker.junit.generators.LongArrayGen
                 com.lmax.undertaker.junit.Source])
  (:import (org.junit.runners.model Statement)
           (org.junit.runner Description)
           (java.util List ArrayList)
           (java.util.function Function)
           (java.lang.reflect Modifier)
           (com.lmax.undertaker.junit Seed Trials))
  (:require [undertaker.core :as undertaker]
            [undertaker.source :as source]
            [clojure.string :as str]))

(defn add-tag-meta-if-applicable [symbol ^Class type]
  (if (and (.isPrimitive type)
           (not= "long" (str type))
           (not= "double" (str type)))
    symbol
    (with-meta symbol {:tag (.getName type)})))

(defmacro override-delegate
  "Based on https://stackoverflow.com/a/33463302/5776097"
  [type delegate & body]
  (let [d (gensym)
        overrides (group-by first body)
        methods (for [m (.getMethods (resolve type))
                      :let [f (-> (.getName m)
                                  symbol
                                  (add-tag-meta-if-applicable (.getReturnType m)))]
                      :when (and (not (overrides f))
                                 (not (Modifier/isPrivate (.getModifiers m)))
                                 (not (Modifier/isProtected (.getModifiers m))))
                      :let [args (for [t (.getParameterTypes m)]
                                   (add-tag-meta-if-applicable (gensym) t))]]
                  (list f (vec (conj args 'this))
                        `(. ~d ~f ~@(map #(with-meta % nil) args))))
        grouped-methods (->> methods
                             (group-by first)
                             (map (fn [[f arities]]
                                    (let [by-arity-length (group-by (fn [[_ args-vec]] (count args-vec)) arities)]
                                      (apply list f (->> by-arity-length
                                                         (map (fn [[k arities]] (first arities)))
                                                         (map (fn [[_ args-vec body]] `(~args-vec (~@body)))))))))
                             (into []))]
    `(let [~d ~delegate]
       (proxy [~type] [] ~@body ~@grouped-methods))))

(defn get-annotation-value [^Class annotation ^Description description default]
  (let [annotation (or (.getAnnotation description annotation)
                       (.getAnnotation (.getTestClass description) annotation))]
    (or (some-> annotation
                (.value))
        default)))

(defn ^Statement -apply [this ^Statement base ^Description description]
  (proxy [Statement] []
    (evaluate []
      (let [seed (get-annotation-value Seed description (undertaker/next-seed (System/nanoTime)))
            trials (get-annotation-value Trials description 1000)
            result (undertaker/run-prop {:seed       seed
                                         :iterations trials} #(.evaluate base))]
        (when (false? (::undertaker/result result))
          (let [test-name (first (str/split (.getDisplayName description) #"\("))
                message (undertaker/format-results test-name result)]
            (throw (override-delegate
                     java.lang.Throwable
                     (::undertaker/cause result)
                     (getMessage [] message)))))))))

(defn ^long -pushInterval [_ ^String interval-name]
  (source/push-interval undertaker/*source* interval-name))

(defn -popInterval [_ ^long interval-id generated-value]
  (source/pop-interval undertaker/*source* interval-id generated-value))

(defn ^byte -getByte
  ([this] (-getByte this Byte/MIN_VALUE Byte/MAX_VALUE))
  ([this max] (-getByte this Byte/MIN_VALUE max))
  ([this min max] (undertaker/byte min max)))

(defn ^short -getShort
  ([this] (-getShort this Short/MIN_VALUE Short/MAX_VALUE))
  ([this max] (-getShort this Integer/MIN_VALUE max))
  ([this min max] (undertaker/short min max)))

(defn ^int -getInt
  ([this] (-getInt this Integer/MIN_VALUE Integer/MAX_VALUE))
  ([this max] (-getInt this Integer/MIN_VALUE max))
  ([this min max] (undertaker/int min max)))

(defn ^long -getLong
  ([this] (-getLong this Long/MIN_VALUE Long/MAX_VALUE))
  ([this max] (-getLong this Long/MIN_VALUE max))
  ([this min max] (undertaker/long min max)))

(defn ^boolean -getBool
  ([this] (undertaker/bool)))

(defn ^char -getChar
  ([this] (undertaker/char)))

(defn ^float -getFloat
  ([this] (-getFloat this (- Float/MAX_VALUE) Float/MAX_VALUE))
  ([this max] (-getFloat this (- Float/MAX_VALUE) max))
  ([this min max] (undertaker/float min max)))

(defn ^double -getDouble
  ([this] (-getDouble this (- Double/MAX_VALUE) Double/MAX_VALUE))
  ([this max] (-getDouble this (- Double/MAX_VALUE) max))
  ([this min max] (undertaker/double min max)))

(defn ^List -getList
  ([this ^Function generator]
   (let [result-vec (undertaker/vec-of #(.apply generator this))
         result-list (ArrayList. (count result-vec))]
     (->> result-vec
          (map #(.add result-list %1))
          (dorun))
     result-vec)))

(defn #^"[J" -getLongArray
  ([this] (long-array (undertaker/vec-of undertaker/long)))
  ([this ^Function generator] (long-array (undertaker/vec-of #(.apply generator this))))
  ([this ^Function generator min] (long-array (undertaker/vec-of #(.apply generator this) min (+ min 64))))
  ([this ^Function generator min max] (long-array (undertaker/vec-of #(.apply generator this) min max))))
