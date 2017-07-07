(ns undertaker.source-rule
  (:gen-class
    :name undertaker.SourceRule
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

(defn ^Statement -apply [{:keys [source]} ^Statement base ^Description description]
  (proxy [Statement] []
    (evaluate []
      (undertaker/run-prop {} (fn [_] (.evaluate base))))))

(defn ^int -getInt [{:keys [source]} min max]
  (undertaker/int source min max))
