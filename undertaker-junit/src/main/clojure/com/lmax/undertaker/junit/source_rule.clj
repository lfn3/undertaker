(ns com.lmax.undertaker.junit.source-rule
  (:gen-class
    :name com.lmax.undertaker.junit.SourceRule
    :state state
    :implements [org.junit.rules.TestRule
                 com.lmax.undertaker.junit.sources.ByteSource
                 com.lmax.undertaker.junit.sources.IntSource
                 com.lmax.undertaker.junit.sources.LongSource
                 com.lmax.undertaker.junit.sources.CharSource
                 com.lmax.undertaker.junit.sources.DoubleSource
                 com.lmax.undertaker.junit.sources.FloatSource
                 com.lmax.undertaker.junit.sources.ShortSource
                 com.lmax.undertaker.junit.sources.BoolSource
                 com.lmax.undertaker.junit.sources.ListSource
                 com.lmax.undertaker.junit.sources.LongArraySource
                 com.lmax.undertaker.junit.Source])
  (:import (org.junit.runners.model Statement)
           (org.junit.runner Description JUnitCore Computer Request)
           (java.util List ArrayList Arrays)
           (java.util.function Function Supplier)
           (java.lang.reflect Modifier)
           (com.lmax.undertaker.junit Seed Trials)
           (com.lmax.undertaker.junit Generator)
           (com.lmax.undertaker.junit.generators IntGenerator))
  (:require [undertaker.core :as undertaker]
            [undertaker.source :as source]
            [clojure.string :as str]))

(def ^:dynamic *nested* false)

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
      (if (not *nested*)                                    ;Check we're not already inside this rule
        (let [seed (get-annotation-value Seed description (undertaker/next-seed (System/nanoTime)))
              trials (get-annotation-value Trials description 1000)
              junit (JUnitCore.)
              computer (Computer.)
              class (resolve (symbol (.getClassName description)))
              test-request (Request/method class (.getMethodName description))
              result (undertaker/run-prop {:seed       seed
                                           :iterations trials}
                                          #(with-bindings {#'*nested* true}
                                             (->> (.run junit test-request)
                                                  (.getFailures)
                                                  (map (fn [f] (-> (.getException f) ;TODO: process failures in a function
                                                                   (throw))))
                                                  (dorun))))]
          (when (false? (::undertaker/result result))
            (let [test-name (first (str/split (.getDisplayName description) #"\("))
                  message (undertaker/format-results test-name result)]
              (throw (override-delegate
                       java.lang.Throwable
                       (::undertaker/cause result)
                       (getMessage [] message))))))
        (.evaluate base)))))

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
  ([this min max] (undertaker/int min max))
  ([this min max & more-ranges] (apply undertaker/int min max more-ranges)))

(defn ^long -getLong
  ([this] (-getLong this Long/MIN_VALUE Long/MAX_VALUE))
  ([this max] (-getLong this Long/MIN_VALUE max))
  ([this min max] (undertaker/long min max)))

(defn ^boolean -getBool
  ([this] (undertaker/bool)))

(defn ^char -getChar
  ([this] (undertaker/char)))

(defn ^char -getAsciiChar
  ([this] (undertaker/char-ascii)))

(defn ^char -getAlphanumericChar
  ([this] (undertaker/char-alphanumeric)))

(defn ^char -getAlphaChar
  ([this] (undertaker/char-alpha)))

(defn ^String -getString
  ([this] (undertaker/string))
  ([this ^IntGenerator intGen] (-getString this intGen 0 undertaker/default-string-max-size))
  ([this ^IntGenerator intGen max] (-getString this intGen 0 max))
  ([this ^IntGenerator intGen min max]
   (->> (undertaker/vec-of intGen min max)
        (map char)
        (char-array)
        (String.))))

(defn ^String -getAsciiString
  ([this] (undertaker/string-ascii))
  ([this max] (undertaker/string-ascii 0 max))
  ([this min max] (undertaker/string-ascii min max)))

(defn ^String -getAlphanumericString
  ([this] (undertaker/string-alphanumeric))
  ([this max] (undertaker/string-alphanumeric 0 max))
  ([this min max] (undertaker/string-alphanumeric min max)))

(defn ^String -getAlphaString
  ([this] (undertaker/string-alpha))
  ([this max] (undertaker/string-alpha 0 max))
  ([this min max] (undertaker/string-alpha min max)))

(defn ^float -getFloat
  ([this] (-getFloat this (- Float/MAX_VALUE) Float/MAX_VALUE))
  ([this max] (-getFloat this (- Float/MAX_VALUE) max))
  ([this min max] (undertaker/float min max)))

(defn ^double -getDouble
  ([this] (-getDouble this (- Double/MAX_VALUE) Double/MAX_VALUE))
  ([this max] (-getDouble this (- Double/MAX_VALUE) max))
  ([this min max] (undertaker/double min max)))

(defn ^double -getRealDouble
  ([this] (-getRealDouble this (- Double/MAX_VALUE) Double/MAX_VALUE))
  ([this max] (-getRealDouble this (- Double/MAX_VALUE) max))
  ([this min max] (undertaker/real-double min max)))

(defn ^List -getList
  ([this ^Function generator] (-getList this generator 0 64))
  ([this ^Function generator max] (-getList this generator 0 max))
  ([this ^Function generator min max] (ArrayList. (undertaker/vec-of #(.apply generator this) min max))))

(defn -getArray
  ([this ^Class c ^Function generator] (-getArray this c generator 0 64))
  ([this ^Class c ^Function generator max] (-getArray this c generator 0 max))
  ([this ^Class c ^Function generator min max] (into-array c (undertaker/vec-of #(.apply generator this) min max))))

(defn -generate
  ([this ^Generator g] (.apply g this)))

(defmacro get-array-fn [type-hint type-str & [array-fn-name]]
  (let [fn-name (symbol (str "-get" (str/upper-case (first type-str))
                             (apply str (rest type-str))
                             "Array"))
        array-fn-name (if array-fn-name
                        (symbol array-fn-name)
                        (symbol (str type-str "-array")))
        generator-name (symbol "undertaker" type-str)]
    `(defn ^{:tag type-hint} ~fn-name
       ([_#] (~array-fn-name (undertaker/vec-of ~generator-name)))
       ([this# ^java.util.function.Function generator#]
         (~array-fn-name (undertaker/vec-of #(.apply generator# this#))))
       ([this# ^java.util.function.Function generator# min#]
         (~array-fn-name (undertaker/vec-of #(.apply generator# this#)) min (+ min 64)))
       ([this# ^java.util.function.Function generator# min# max#]
         (~array-fn-name (undertaker/vec-of #(.apply generator# this#)) min max)))))

(get-array-fn "[J" "long")
(get-array-fn "[B" "byte")
(get-array-fn "[C" "char")
(get-array-fn "[D" "double")
(get-array-fn "[F" "float")
(get-array-fn "[I" "int")
(get-array-fn "[S" "short")
(get-array-fn "[Z" "bool" "boolean-array")
