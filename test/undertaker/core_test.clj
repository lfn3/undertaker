(ns undertaker.core-test
  (:require [clojure.test :as t :refer [deftest is]]
            [clojure.spec.test.alpha :as spec-test]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [undertaker.core :as undertaker]))

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
        faliures (->> result
                      (filter #(-> %1
                                   (get-in [:clojure.spec.test.check/ret :result])
                                   (not)
                                   (true?))))]
    (println (str "Checked following specs in " target-namespace ": " ))
    (dorun (map println targets))
    (is (empty? faliures))))

(deftest test-boolean-gen
  (is (boolean? (undertaker/bool-gen))))

(deftest test-int-gen
  (is (integer? (undertaker/int-gen))))

(deftest test-vec-gen
  (is (vector? (undertaker/vec-gen undertaker/int-gen))))
