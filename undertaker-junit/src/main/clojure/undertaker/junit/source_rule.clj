(ns undertaker.junit.source-rule
  (:gen-class
    :name undertaker.junit.SourceRule
    :state state
    :init init
    :implements [org.junit.rules.TestRule
                 undertaker.junit.generators.ByteGen
                 undertaker.junit.generators.IntGen
                 undertaker.junit.generators.BoolGen])
  (:import (org.junit.runners.model Statement)
           (org.junit.runner Description))
  (:require [undertaker.core :as undertaker]))

(defn -init []
  [[] (let [seed (System/nanoTime)]
        {:seed   seed
         :source (undertaker.source.wrapped-random/make-source seed)})])

(defn ^Statement -apply [this ^Statement base ^Description description]
  (let [state (.state this)]
    (proxy [Statement] []
      (evaluate []
        (let [{:keys [source seed]} state
              result (undertaker/run-prop {::undertaker/seed seed} source (fn [_] (.evaluate base)))]
          (when (false? (::undertaker/result result))
            (throw (ex-info "Test failed" result (::undertaker/ex result)))))))))

(defn get-source [this]
  (-> this
      (.state)
      :source))

(defn ^byte -getByte
  ([this] (-getByte this Byte/MIN_VALUE Byte/MAX_VALUE))
  ([this max] (-getByte this Byte/MIN_VALUE max))
  ([this min max] (-> this
                      (get-source)
                      (undertaker/byte min max))))

(defn ^int -getInt
  ([this] (-getInt this Integer/MIN_VALUE Integer/MAX_VALUE))
  ([this max] (-getInt this Integer/MIN_VALUE max))
  ([this min max] (-> this
                      (get-source)
                      (undertaker/int min max))))

(defn ^boolean -getBool
  ([this] (-> this
              (get-source)
              (undertaker/bool))))