(ns undertaker.junit.source-rule
  (:gen-class
    :name undertaker.junit.SourceRule
    :state state
    :init init
    :implements [org.junit.rules.TestRule undertaker.generators.IntGen])
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

(defn ^int -getInt
  ([this min max] (let [{:keys [source seed]} (.state this)]
                    (undertaker/int source min max))))
