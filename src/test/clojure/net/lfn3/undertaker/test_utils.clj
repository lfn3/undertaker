(ns net.lfn3.undertaker.test-utils
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as s.test]
            [orchestra.spec.test :as orchestra.test]
            [net.lfn3.undertaker.source :as source]
            [net.lfn3.undertaker.source.wrapped-random :as source.random]
            [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.core :as undertaker]
            [clojure.test :as t]))

(defn each-fixtures [f]
  (let [sources (atom [])]
    (try
      (with-redefs {#'source.random/make-source #(let [source (apply source.random/make-source %)]
                                                   (swap! sources conj source)
                                                   source)}
        (f))
      (finally

        (is (->> @sources
                 (map source/get-state)
                (map ::proto/interval-stack)
                (every? empty?)))
        (reset! sources [])

        (is (->> undertaker/*source*
                 (source/get-state)
                 ::proto/interval-stack
                 (empty?)))

        (source/reset undertaker/*source*)))))

(defn once-fixtures [f]
  (orchestra.test/instrument)
  (f)
  (orchestra.test/unstrument))

(defn use-standard-fixtures []
  (t/use-fixtures :once once-fixtures)
  (t/use-fixtures :each each-fixtures))

(defmacro defchecks
  ([target-ns] `(defchecks ~target-ns #{}))
  ([target-ns ignored]
   (loop [targets (->> (s/registry)
                       (map key)
                       (filter #(str/starts-with? (str %1) (str target-ns)))
                       (remove ignored))
          result '()]
     (let [to-check (first targets)]
       (if to-check
         (let [test-sym (symbol (str "check-" (str/replace to-check #"[/\.]" "-")))
               test `(deftest ~test-sym
                       (let [result# (s.test/check (quote ~to-check) {:clojure.spec.test.check/opts {:num-tests 100}})
                             passed?# (get-in (first result#) [:clojure.spec.test.check/ret :result])]
                         (is (true? passed?#) result#)))]
           (recur (rest targets) (conj result test)))
         (conj result `do))))))
