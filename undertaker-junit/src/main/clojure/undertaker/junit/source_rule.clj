(ns undertaker.junit.source-rule
  (:gen-class
    :name undertaker.junit.SourceRule
    :state state
    :init init
    :implements [org.junit.rules.TestRule
                 undertaker.junit.generators.ByteGen
                 undertaker.junit.generators.IntGen
                 undertaker.junit.generators.LongGen
                 undertaker.junit.generators.BoolGen
                 undertaker.junit.generators.ListGen
                 undertaker.junit.Source])
  (:import (org.junit.runners.model Statement)
           (org.junit.runner Description)
           (java.util List ArrayList)
           (java.util.function Function))
  (:require [undertaker.core :as undertaker]
            [undertaker.source :as source]))

(defn -init []
  [[] (let [seed (System/nanoTime)]
        {:seed   seed})])

(defn ^Statement -apply [this ^Statement base ^Description description]
  (let [state (.state this)]
    (proxy [Statement] []
      (evaluate []
        (let [{:keys [source seed]} state
              result (undertaker/run-prop {::undertaker/seed seed} #(.evaluate base))]
          (when (false? (::undertaker/result result))
            (throw (ex-info "Test failed" result (::undertaker/ex result)))))))))

(defn ^long -pushInterval [_ ^String interval-name]
  (source/push-interval undertaker/*source* interval-name))

(defn -popInterval [_ ^long interval-id generated-value]
  (source/pop-interval undertaker/*source* interval-id generated-value))

(defn ^byte -getByte
  ([this] (-getByte this Byte/MIN_VALUE Byte/MAX_VALUE))
  ([this max] (-getByte this Byte/MIN_VALUE max))
  ([this min max] (undertaker/byte min max)))

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

(defn ^List -getList
  ([this ^Function generator]
   (let [result-vec (undertaker/vec-of #(.apply generator this))
         result-list (ArrayList. (count result-vec))]
     (->> result-vec
          (map #(.add result-list %1))
          (dorun))
     result-vec)))
