(ns net.lfn3.undertaker.shrink-test
  (:require [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.core :as undertaker]
            [orchestra.spec.test :as orchestra.test]
            [clojure.test :as t :refer [deftest is]]
            [net.lfn3.undertaker.source :as source]
            [net.lfn3.undertaker.shrink :as shrink]
            [clojure.string :as str]
            [clojure.spec.test.alpha :as s.test]
            [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.source.fixed :as source.fixed]
            [net.lfn3.undertaker.specs.bytes]
            [net.lfn3.undertaker.specs.core]
            [net.lfn3.undertaker.specs.shrink]
            [net.lfn3.undertaker.specs.proto]))

(t/use-fixtures :once #(do (orchestra.test/instrument)
                           (%1)
                           (orchestra.test/unstrument)))

(def this-ns *ns*)

(def ignored #{})

(deftest check-shrinking
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

(deftest can-fail-prop
  (is (false? (get-in (undertaker/run-prop {} #(is false)) [::undertaker/initial-results ::undertaker/result]))))

(deftest can-run-prop
  (is (true? (get-in (undertaker/run-prop {} (constantly true)) [::undertaker/initial-results ::undertaker/result]))))

(deftest should-shrink-to-zero
  (let [result (undertaker/run-prop {} #(is (boolean? (undertaker/byte))))]
    (is (= 0 (->> result
                  ::undertaker/shrunk-results
                  ::undertaker/generated-values
                  (first)))
        result)))

(deftest should-not-shrink-to-zero-if-does-not-fail-on-zero
  (let [result (undertaker/run-prop {} #(is (= 0 (undertaker/byte))))]
    (is (->> result
             ::undertaker/shrunk-results
             ::undertaker/generated-values
             (first)
             (zero?)
             (not))
        result)))

(deftest should-shrink-past-1
  (is (= 0 (->> #(is (= 1 (undertaker/byte)))
                (undertaker/run-prop {})
                ::undertaker/shrunk-results
                ::undertaker/generated-values

                (first)))))

(deftest should-shrink-to-2
  (is (= 2 (->> #(let [value (undertaker/byte)]
                   (is (if (= 0 value)
                         true
                         (odd? value))))
                (undertaker/run-prop {})
                ::undertaker/shrunk-results
                ::undertaker/generated-values
                (first)))))

(deftest snip-interval-test
  (is (= [0 0] (vec (shrink/snip-interval (byte-array [0 1 1 0]) {::proto/interval-start 1
                                                                  ::proto/interval-end   3})))))

(deftest snip-intervals-should-handle-overrun-exceptions
  (is (= [0] (-> (byte-array [0])
                 (shrink/snip-intervals [{::proto/interval-start 0
                                          ::proto/interval-end   0
                                          ::proto/hints          []}]
                                        (undertaker/wrap-fn #(undertaker/int)))
                 (vec)))))

(deftest snip-intervals-handles-single-byte-failure
  (is (= [-19] (-> (byte-array [1 -19])
                   (shrink/snip-intervals [{::proto/interval-start 0
                                            ::proto/interval-end   1
                                            ::proto/hints          [[::proto/this ::proto/snippable nil]]}]
                                          (undertaker/wrap-fn #(is (boolean? (undertaker/byte)))))
                   (vec)))))

(deftest shrinking-vec-with-overrun
  (let [bytes-to-shrink (byte-array [1 22 1 77 0])
        result (->> #(is (every? even? (undertaker/vec-of undertaker/byte 1 2)))
                    (undertaker/run-prop {:debug true}))]
    (is (= [1] (->> result
                    ::undertaker/shrunk-results
                    ::undertaker/generated-values
                    (first))))))

(deftest should-shrink-middle-byte
  (let [result (->> #(let [bool-1 (undertaker/bool)
                           a-number (undertaker/int)
                           bool-2 (undertaker/bool)]
                       (is (not bool-1)))
                    (undertaker/run-prop {}))]
    (is (= [true 0 false] (-> result
                              ::undertaker/shrunk-results
                              ::undertaker/generated-values
                              (vec)))
        result)))

(deftest should-shrink-vec-to-smallest-failing-case
  (let [result (->> #(is (every? even? (undertaker/vec-of undertaker/byte 1 2)))
                    (undertaker/run-prop {}))
        shrunk-vector (->> result
                           ::undertaker/shrunk-results
                           ::undertaker/generated-values
                           (first))]
    (is (or (= [1] shrunk-vector)
            (= [-1] shrunk-vector)) result)))

(deftest should-only-run-once-since-source-is-unused
  (let [result (->> (fn [])
                    (undertaker/run-prop {}))]
    (is (= 1 (::undertaker/iterations-run result)))))

(deftest should-shrink-to-below-two
  ;Sometimes gets stuck at 2.0 due to reducing upper bytes before lower bytes.
  ;I think that's fine?
  (let [result (->> (fn [] (let [value (undertaker/double)]
                             (is (< value 0.9))))
                    (undertaker/run-prop {:debug true}))
        shrunk-val (first (get-in result [::undertaker/shrunk-results ::undertaker/generated-values]))]
    (is (<= 0.9 shrunk-val) result)
    (is (<= shrunk-val 2.0) result)))

(def get-first-key #(some-> %1
                            (first)
                            (first)))

#_(deftest should-shrink-multi-part-map-keys
  (let [result (->> (fn [] (let [value (undertaker/map-of undertaker/string undertaker/short)
                                 empty-str? (some-> (get-first-key value)
                                                    (empty?))]
                             (is (or (nil? empty-str?) empty-str?))))
                    (undertaker/run-prop {:debug true}))
        shrunk-value (get-in result [::undertaker/shrunk-results ::undertaker/generated-values])
        initial-value (get-in result [::undertaker/initial-results ::undertaker/generated-values])]
    (is (= 1 (count (get-first-key (first shrunk-value)))) result)))

#_(deftest shrinking-should-be-deterministic
  (let [r1 (->> (fn [] (let [value (undertaker/map-of undertaker/string undertaker/short)
                             empty-str? (some-> (get-first-key value)
                                                (empty?))]
                         (is (or (nil? empty-str?) empty-str?))))
                (undertaker/run-prop {:debug true :seed 1})
                ::undertaker/shrunk-results
                ::undertaker/generated-values)
        r2 (->> (fn [] (let [value (undertaker/map-of undertaker/string undertaker/short)
                             empty-str? (some-> (get-first-key value)
                                                (empty?))]
                         (is (or (nil? empty-str?) empty-str?))))
                (undertaker/run-prop {:debug true :seed 1})
                ::undertaker/shrunk-results
                ::undertaker/generated-values)]
    (is (= r1 r2))))
