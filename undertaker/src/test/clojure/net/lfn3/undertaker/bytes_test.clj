(ns net.lfn3.undertaker.bytes-test
  (:require [clojure.test :refer [deftest is] :as t]
            [net.lfn3.undertaker.bytes :refer :all]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as s.test]
            [orchestra.spec.test :as orchestra.test])
  (:import (java.nio ByteBuffer)))

(t/use-fixtures :once #(do (orchestra.test/instrument)
                           (%1)
                           (orchestra.test/unstrument)))

(def this-ns *ns*)

(def ignored #{})

(deftest check-bytes
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


(deftest unsigned<=-test
  (is (unsigned<= 1 1))
  (is (unsigned<= 0 1))
  (is (unsigned<= 0 -1))
  (is (unsigned<= 0 -128))
  (is (unsigned<= 127 -128))
  (is (not (unsigned<= 1 0)))
  (is (not (unsigned<= -1 0)))
  (is (not (unsigned<= -128 0)))
  (is (not (unsigned<= -128 127))))

(deftest unsigned-range-test
  (is (= 0 (unsigned-range 0 0)))
  (is (= 127 (unsigned-range -128 -1)))
  (is (= 127 (unsigned-range 0 127)))
  (is (= 128 (unsigned-range 63 -65)))
  (is (= 255 (unsigned-range 0 -1))))

(deftest test-move-into-range
  (is (= 0 (move-into-range 0 0 0)))
  (is (= 0 (move-into-range 12 0 0)))
  (is (= 0 (move-into-range 0 0 12)))
  (is (= 1 (move-into-range 0 1 12)))
  (is (= 12 (move-into-range 12 0 12)))
  (is (= 0 (move-into-range 0 0 12)))
  (is (= 0 (move-into-range 4 0 3)))
  (is (= 3 (move-into-range 7 0 3)))
  (is (= 3 (move-into-range 7 3 3))))

(deftest test-is-in-range
  (is (true? (is-in-range 0 [0 0])))
  (is (true? (is-in-range 0 [0 1])))
  (is (false? (is-in-range 0 [-128 -1]))))

(deftest test-is-in-ranges
  (is (= '([0 0]) (is-in-ranges 0 [[0 0]])))
  (is (= nil (is-in-ranges 1 [[0 0]])))
  (is (= '([0 1] [0 1]) (is-in-ranges 1 [[0 1] [0 1]]))))

(deftest test-values-in-range?
  (is (true? (values-in-range? [0 0] [[0 0] [0 1]])))
  (is (true? (values-in-range? [-1 -1] [[-128 -128] [-1 -1]])))
  (is (true? (values-in-range? [0] [[0 5] [0 10]])))
  (is (true? (values-in-range? [-1] [[-1 -17 -1 -1 -1 -1 -1 -1] [-65 -16 0 0 0 0 0 0]]))))

(deftest test-ranges-at-idx
  (is (= [[0 0]] (map (partial map #(nth %1 0)) [[[0] [0]]])))
  (is (= [[1 -1]] (map (partial map #(nth %1 1)) [[[0 1] [0 -1]]]))))

(defn vectorized-move-bytes-into-range
  ([input min max] (vectorized-move-bytes-into-range input min max #{}))
  ([input min max skip]
   (let [ranges (split-number-line-min-max-into-bytewise-min-max min max short->bytes)
         skip-bytes (set (map short->bytes skip))]
     (-> input
         (byte-array)
         (ByteBuffer/wrap)
         (map-into-ranges! ranges skip-bytes)
         (.getShort)))))

(deftest test-split-number-line-min-max-into-bytewise-min-max
  (let [vectorized #(->> (split-number-line-min-max-into-bytewise-min-max %1 %2 short->bytes)
                         (map (partial map vec))
                         (map vec)
                         (vec))]
    (is (= [[[0 0] [0 1]]] (vectorized 0 1)))
    (is (= [[[-1 -1] [-1 -1]] [[0 0] [0 1]]] (vectorized -1 1)))
    (is (= [[[0 0] [0 0]]] (vectorized 0 0)))))

(deftest test-split-number-line-ranges-into-bytewise-min-max
  (let [vectorized (fn [& args] (->> (split-number-line-ranges-into-bytewise-min-max args short->bytes)
                                     (map (partial map vec))
                                     (map vec)
                                     (vec)))]
    (is (= [[[0 0] [0 1]]] (vectorized 0 1)))
    (is (= [[[-1 -1] [-1 -1]] [[0 0] [0 1]]] (vectorized -1 1)))
    (is (= [[[0 0] [0 1]] [[0 3] [0 4]]] (vectorized 0 1 3 4)))
    (is (= [[[-1 -1] [-1 -1]] [[0 0] [0 1]] [[0 3] [0 4]]] (vectorized -1 1 3 4)))
    (is (= [[[-1 -4] [-1 -3]] [[-1 -1] [-1 -1]] [[0 0] [0 1]] [[0 3] [0 4]]] (vectorized -4 -3 -1 1 3 4)))))

(deftest double-arrays-examples
  (is (= (vec (double->bytes -2.563353952042129E75))
         [-49 -106 -85 58 73 -49 -24 -102]))                ;This is the problem with the generator. -65 < -49 < 63.
  (is (= (vec (double->bytes 1.0))                          ;But if we consider them as unsigned, -49 > -65.
         [63 -16 0 0 0 0 0 0]))
  (is (= (vec (double->bytes -1.0))
         [-65 -16 0 0 0 0 0 0]))
  (is (= (vec (double->bytes 0.5))
         [63 -32 0 0 0 0 0 0]))
  (is (= (vec (double->bytes -0.5))
         [-65 -32 0 0 0 0 0 0]))
  (is (= (vec (double->bytes 0.2))
         [63 -55 -103 -103 -103 -103 -103 -102]))
  (is (= (vec (double->bytes 1.0000000000000002))
         [63 -16 0 0 0 0 0 1])))

#_(deftest test-skip-disallowed-values
  (is (= -128 (skip-disallowed-values 127 #{(byte-array [127])} )))
  (is (= -2 (skip-disallowed-values -2 #{(byte-array [127])} )))
  (is (= 6 (skip-disallowed-values 5 #{(byte-array [1])
                                     (byte-array [5])} )))
  (is (= 4 (skip-disallowed-values 4 #{(byte-array [1])
                                     (byte-array [5])}))))

(deftest test-punch-skip-values-out-of-range
  (is (= (punch-skip-values-out-of-ranges [[1]] [[[1] [4]]]) [[[2] [4]]]))
  (is (= (punch-skip-values-out-of-ranges [[2]] [[[1] [4]]]) [[[1] [1]] [[3] [4]]]))
  (is (= (punch-skip-values-out-of-ranges [[2]] [[[4] [80]]]) [[[4] [80]]]))
  (is (= (punch-skip-values-out-of-ranges [[0]] [[[0 0] [2 2]]]) [[[1 0] [2 2]]]))
  (is (= (punch-skip-values-out-of-ranges [[0]] [[[0 1] [2 2]]]) [[[1 0] [2 2]]]))
  (is (= (punch-skip-values-out-of-ranges [[0 0]] [[[0 0 0] [2 2 2]]]) [[[0 1 0] [2 2 2]]]))
  (is (= (punch-skip-values-out-of-ranges [[1 0]] [[[0 0 0] [2 2 2]]]) [[[0 0 0] [0 -1 -1]] [[1 1 0] [2 2 2]]]))
  (is (= (punch-skip-values-out-of-ranges [[0 0 0 3]] [[[0 0 0 1] [0 0 0 3]]]) [[[0 0 0 1] [0 0 0 2]]])))

(deftest test-range<
  (is (true? (range< [[1] [1]] [[2] [2]])))
  (is (false? (range< [[1] [1]] [[1] [2]])))
  (is (false? (range< [[1] [2]] [[1] [2]])))

  (is (true? (range< [(byte-array [1]) (byte-array [1])] [(byte-array [2]) (byte-array [2])])))
  (is (false? (range< [(byte-array [1]) (byte-array [1])] [(byte-array [1]) (byte-array [2])])))
  (is (false? (range< [(byte-array [1]) (byte-array [2])] [(byte-array [1]) (byte-array [2])]))))

(deftest test-ranges-are-sorted
  (let [ranges-are-sorted? #(ranges-are-sorted? %1 range<)]
    (is (true? (ranges-are-sorted? [[[1] [1]] [[2] [2]]])))
    (is (true? (ranges-are-sorted? [[[1] [1]] [[2] [2]]
                                    [[3] [3]] [[4] [4]]])))
    (is (false? (ranges-are-sorted? [[[3] [3]] [[4] [4]]
                                     [[1] [1]] [[2] [2]]])))

    (is (true? (ranges-are-sorted? [[(byte-array [1]) (byte-array [1])] [(byte-array [2]) (byte-array [2])]])))
    (is (true? (ranges-are-sorted? [[(byte-array [1]) (byte-array [1])] [(byte-array [2]) (byte-array [2])]
                                    [(byte-array [3]) (byte-array [3])] [(byte-array [4]) (byte-array [4])]])))
    (is (false? (ranges-are-sorted? [[(byte-array [3]) (byte-array [3])] [(byte-array [4]) (byte-array [4])]
                                     [(byte-array [1]) (byte-array [1])] [(byte-array [2]) (byte-array [2])]])))))

(deftest test-map-into-ranges
  (let [make-short vectorized-move-bytes-into-range]
    (is (= 0 (make-short [0 0] 0 1)))
    (is (= 127 (make-short [0 127] 0 127)))
    (is (= 5 (make-short [0 0] 5 10)))
    (is (= 6 (make-short [0 1] 5 10)))
    (is (= 7 (make-short [0 2] 5 10)))
    (is (= 8 (make-short [0 3] 5 10)))
    (is (= 9 (make-short [0 4] 5 10)))
    (is (= 5 (make-short [0 5] 5 10)))
    (is (= 6 (make-short [0 6] 5 10)))
    (is (= 6 (make-short [22 6] 5 10)))

    (is (= 258 (make-short [1 2] 257 514)))
    (is (= 257 (make-short [1 0] 257 514)))
    (is (= 257 (make-short [1 1] 257 514)))
    (is (= 259 (make-short [1 3] 257 514)))
    (is (= 510 (make-short [1 -2] 257 514)))

    (is (= 0 (make-short [0 0] -1 1)))
    (is (= 1 (make-short [0 1] -1 1)))
    (is (= 0 (make-short [0 2] -1 1)))
    (is (= -1 (make-short [-1 -1] -1 1)))
    (is (= -1 (make-short [-2 -2] -1 1)))
    (is (= -1 (make-short [-2 -3] -1 1)))

    (is (= -1 (make-short [-1 -1] -10 1)))
    (is (= -2 (make-short [-1 -2] -10 1)))
    (is (= -10 (make-short [-1 -10] -10 1)))
    (is (= -5 (make-short [-1 -11] -10 1)))
    (is (= -10 (make-short [-1 -16] -10 1)))
    (is (= -1 (make-short [-1 -17] -10 1)))
    (is (= -10 (make-short [-1 -26] -10 1)))

    (is (not= -5 (make-short [-1 -1] -1 1 #{-5})))
    (is (not= -5 (make-short [-1 -1] -10 1 #{-5})))
    (is (not= -5 (make-short [-1 -2] -10 1 #{-5})))
    (is (not= -5 (make-short [-1 -10] -10 1 #{-5})))
    (is (not= -5 (make-short [-1 -11] -10 1 #{-5})))
    (is (not= -5 (make-short [-1 -18] -10 1 #{-5})))
    (is (not= -5 (make-short [-1 -15] -10 1 #{-5})))
    (is (not= -5 (make-short [-1 -16] -10 1 #{-5})))
    (is (not= -5 (make-short [-1 -17] -10 1 #{-5})))
    (is (not= -5 (make-short [-1 -18] -10 1 #{-5})))
    (is (not= -5 (make-short [-1 -19] -10 1 #{-5})))
    (is (not= -5 (make-short [-1 -25] -10 1 #{-5})))))
