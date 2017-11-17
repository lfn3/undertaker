(ns undertaker.shrink-test
  (:require [undertaker.proto :as proto]
            [undertaker.core :as undertaker]
            [orchestra.spec.test :as orchestra.test]
            [clojure.test :as t :refer [deftest is]]
            [undertaker.source :as source]
            [undertaker.shrink :as shrink]
            [clojure.string :as str]
            [clojure.spec.test.alpha :as s.test]
            [clojure.spec.alpha :as s]
            [undertaker.source.fixed :as source.fixed]))

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
  (is (false? (::undertaker/result (undertaker/run-prop {} #(is false))))))

(deftest can-run-prop
  (is (true? (::undertaker/result (undertaker/run-prop {} (constantly true))))))

(deftest should-shrink-to-zero
  (let [result (undertaker/run-prop {} #(is (boolean? (undertaker/byte))))]
    (is (= 0 (->> result
                  ::undertaker/shrunk-values
                  (first)))
        result)))

(deftest should-not-shrink-to-zero-if-does-not-fail-on-zero-prop
  (let [result (undertaker/run-prop {} #(is (= 0 (undertaker/byte))))]
    (is (->> result
             ::undertaker/shrunk-values
             (first)
             (zero?)
             (not))
        result)))

(deftest should-shrink-two-steps
  (is (= [0] (vec (proto/get-sourced-bytes (shrink/shrink (byte-array [2])
                                                          []
                                                          (constantly {::undertaker/result false})))))))

(deftest should-not-shrink-to-zero-if-does-not-fail-on-zero-shrinker
  (is (not (zero? (-> (shrink/shrink (byte-array [2])
                                     []
                                     (undertaker/wrap-fn (fn [] {::undertaker/result (is (= 0 (undertaker/byte)))})))
                      (proto/get-sourced-bytes)
                      (first))))))

(deftest should-shrink-past-1
  (is (= [0] (-> (shrink/shrink (byte-array [5])
                                []
                                (undertaker/wrap-fn (fn [] {::undertaker/result (is (= 1 (undertaker/byte)))})))
                 (proto/get-sourced-bytes)
                 (vec)))))

(deftest should-shrink-to-2
  (is (= [2] (-> (shrink/shrink (byte-array [80])
                                []
                                (undertaker/wrap-fn (fn [] {::undertaker/result (let [value (undertaker/byte)]
                                                                                  (is (if (= 0 value)
                                                                                        true
                                                                                        (odd? value))))})))
                 (proto/get-sourced-bytes)
                 (vec)))))

(deftest snip-interval-test
  (is (= [0 0] (vec (shrink/snip-interval (byte-array [0 1 1 0]) {::proto/interval-start 1
                                                                  ::proto/interval-end   3})))))

(deftest snip-intervals-should-handle-overrun-exceptions
  (is (= [0] (-> (byte-array [0])
                 (shrink/snip-intervals [{::proto/interval-start 0
                                          ::proto/interval-end   0}]
                                        (undertaker/wrap-fn #(undertaker/int)))
                 (vec)))))

(deftest snip-intervals-handles-single-byte-failure
  (is (= [-19] (-> (byte-array [1 -19])
                   (shrink/snip-intervals [{::proto/interval-start 0
                                            ::proto/interval-end   1
                                            ::proto/hints [[::proto/this ::proto/snippable nil]]}]
                                          (undertaker/wrap-fn #(is (boolean? (undertaker/byte)))))
                   (vec)))))

(deftest shrinking-vec-with-overrun
  (let [bytes-to-shrink (byte-array [1 22 1 77 0])
        test-fn (-> #(is (every? even? (undertaker/vec-of undertaker/byte 1 2)))
                    (undertaker/wrap-fn))
        source (source.fixed/make-fixed-source bytes-to-shrink)
        result (test-fn source)
        intervals (source/get-intervals source)
        shrunk-source (shrink/shrink bytes-to-shrink intervals test-fn)]
    (is (= [1] (->> (source/get-intervals shrunk-source)
                    (filter (comp zero? ::proto/interval-depth))
                    (map ::proto/generated-value)
                    (first))))))

(deftest should-shrink-middle-byte
  (let [result (->> #(let [bool-1 (undertaker/bool)
                           a-number (undertaker/int)
                           bool-2 (undertaker/bool)]
                       (is (not bool-1)))
                    (undertaker/run-prop {}))]
    (is (= [true 0 false] (-> result
                              ::undertaker/shrunk-values
                              (vec)))
        result)))

(deftest should-shrink-vec-to-smallest-failing-case
  (let [result (->> #(is (every? even? (undertaker/vec-of undertaker/byte 1 2)))
                    (undertaker/run-prop {}))
        shrunk-vector (->> result
                           ::undertaker/shrunk-values
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
                    (undertaker/run-prop {}))
        shrunk-val (first (::undertaker/shrunk-values result))]
    (is (<= 0.9 shrunk-val))
    (is (<= shrunk-val 2.0))))
