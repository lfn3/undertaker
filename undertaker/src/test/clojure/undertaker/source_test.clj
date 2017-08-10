(ns undertaker.source-test
  (:require [clojure.test :refer [deftest is] :as t]
            [undertaker.source.forgetful :as source.forgetful]
            [undertaker.source :as source]
            [undertaker.util :as util]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as s.test]))

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
  (let [value (source/get-byte forgetful-source)]
    (is util/byte? value)
    (is (contains? (set (range -128 127)) value))))

(deftest should-emit-negative-number
  (let [value (source/get-byte forgetful-source -128 -1)]
    (is neg-int? value)
    (is (contains? (set (range -128 -1)) value))))

(deftest should-emit-positive-number
  (let [value (source/get-byte forgetful-source 1 127)]
    (is pos-int? value)
    (is (contains? (set (range 1 127)) value))))

(deftest should-emit-numbers-in-range
  (is (= 127 (source/get-byte forgetful-source 127 127)))
  (let [values (repeatedly 10 #(source/get-byte forgetful-source 126 127))]
    (is (not-every? (partial = 126) values))
    (is (not-every? (partial = 127) values)))
  (let [values (repeatedly 10 #(source/get-byte forgetful-source -1 1))]
    (is (every? #(= (Integer/signum %1) %1) values))
    (is (not-every? zero? values))
    (is (not-every? (partial = -1) values))
    (is (not-every? (partial = 1) values))
    (is (seq (filter zero? values)))
    (is (seq (filter (partial = -1) values)))
    (is (seq (filter (partial = 1) values))))
  (let [values (repeatedly 100000 #(source/get-byte forgetful-source -128 127))]
    (is (->> values
             (map (fn [val] [((set (range -128 128)) val) val]))
             (filter (comp nil? first))
             (empty?)))))

(deftest should-emit-unsigned-numbers-in-range
  (is (= 0 (source/get-ubyte forgetful-source 0)))
  (let [values (repeatedly 10 #(source/get-ubyte forgetful-source 1))]
    (is (not-every? (partial = 0) values))
    (is (not-every? (partial = 1) values)))
  (let [values (repeatedly 100000 #(source/get-ubyte forgetful-source -1))]
    (is (->> values
             (map (fn [val] [((set (range -128 128)) val) val]))
             (filter (comp nil? first))
             (empty?)))))
