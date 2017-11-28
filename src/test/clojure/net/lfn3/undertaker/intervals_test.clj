(ns net.lfn3.undertaker.intervals-test
  (:require [clojure.test :refer :all :as t]
            [clojure.spec.test.alpha :as s.test]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [orchestra.spec.test :as orchestra.test]
            [net.lfn3.undertaker.specs.intervals]))


(t/use-fixtures :once #(do (orchestra.test/instrument)
                           (%1)
                           (orchestra.test/unstrument)))

(def this-ns *ns*)

(def ignored #{})

(deftest check-intervals
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
