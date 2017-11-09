(ns undertaker.source-test
  (:require [clojure.test :refer [deftest is] :as t]
            [undertaker.source.forgetful :as source.forgetful]
            [undertaker.source :as source]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as s.test]
            [undertaker.bytes :as bytes]))

(def this-ns *ns*)

(def ignored #{})

(deftest check-source
  (let [target-namespace (first (str/split (str this-ns) #"-test"))
        targets (->> (s/registry)
                     (filter #(str/starts-with? (str (key %1)) target-namespace))
                     (map first)
                     (remove ignored))
        result (s.test/check targets {:clojure.spec.test.check/opts {:num-tests 100}})
        failures (->> result
                      (filter #(-> %1
                                   (get-in [:clojure.spec.test.check/ret :result])
                                   (not)
                                   (true?))))]
    (println (str "Checked following specs in " target-namespace ": "))
    (dorun (map println targets))
    (is (empty? failures))))

(def forgetful-source (source.forgetful/make-source (System/nanoTime)))

(deftest should-emit-bytes
  (let [value (first (source/get-bytes forgetful-source [[[-128] [-1]] [[0] [127]]]))]
    (is bytes/byte? value)
    (is (contains? (set (range -128 127)) value))))

(deftest should-emit-positive-number
  (let [value (first (source/get-bytes forgetful-source [[[0] [127]]]))]
    (is pos-int? value)
    (is (contains? (set (range 0 127)) value))))

(deftest should-emit-unsigned-numbers-in-range
  (is (= 0 (first (source/get-bytes forgetful-source [[[0] [0]]]))))
  (let [values (source/get-bytes forgetful-source [[(vec (repeat 10 0)) (vec (repeat 10 1))]])]
    (is (not-every? (partial = 0) values))
    (is (not-every? (partial = 1) values)))
  (let [size 10000
        values (source/get-bytes forgetful-source [[(vec (repeat size -128)) (vec (repeat size -1))]
                                                   [(vec (repeat size 0)) (vec (repeat size 127))]])]
    (is (->> values
             (map (fn [val] [((set (range -128 128)) val) val]))
             (filter (comp nil? first))
             (empty?)))))
