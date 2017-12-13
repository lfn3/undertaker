(ns net.lfn3.undertaker.test-utils
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as s.test]))

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
