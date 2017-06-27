(ns undertaker.core-test
  (:require [clojure.test :as t :refer [deftest is]]
            [clojure.spec.test.alpha :as spec-test]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [undertaker.core :as undertaker]
            [clojure.test.check :as tcheck]
            [clojure.test.check.clojure-test :as tcheck-test]
            [clojure.test.check.properties :as tcheck-prop]
            [clojure.test.check.generators :as tcheck-gen]
            [undertaker.source :as source]))

(t/use-fixtures :once #(do (spec-test/instrument)
                           (%1)
                           (spec-test/unstrument)))
(t/use-fixtures :each undertaker/fixture)

(def this-ns *ns*)

(def ignored #{})

(deftest check-core
  (let [target-namespace (first (str/split (str this-ns) #"-test"))
        targets (->> (s/registry)
                     (filter #(str/starts-with? (str (key %1)) target-namespace))
                     (map first)
                     (remove ignored))
        result (spec-test/check targets)
        failures (->> result
                      (filter #(-> %1
                                   (get-in [:clojure.spec.test.check/ret :result])
                                   (not)
                                   (true?))))]
    (println (str "Checked following specs in " target-namespace ": " ))
    (dorun (map println targets))
    (is (empty? failures))))

(deftest test-boolean-gen
  (is (boolean? (undertaker/bool-gen))))

(deftest test-int-gen
  (is (integer? (undertaker/int-gen))))

(deftest test-vec-gen
  (is (vector? (undertaker/vec-gen undertaker/int-gen))))

(deftest test-run-and-report
  (t/testing "Failure"
    (is (false? (undertaker/run-and-report (source/make-source 1) (is false)))))
  (t/testing "Success"
    (is (true? (undertaker/run-and-report (source/make-source 1) (is true)))))
  (t/testing "Handles multiple statements"
    (is (false? (undertaker/run-and-report
                  (source/make-source 1)
                  (is true)
                  (is false)
                  (is true))))))

(deftest can-run-prop
  (undertaker/run-prop {} (constantly true)))

(deftest a-prop
  (undertaker/prop {}
    (is false)))
