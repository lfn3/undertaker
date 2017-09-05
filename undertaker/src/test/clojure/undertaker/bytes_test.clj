(ns undertaker.bytes-test
  (:require [clojure.test :refer :all]
            [undertaker.bytes :refer :all])
  (:import (java.nio ByteBuffer)))

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
  (is (= 3 (move-into-range 7 3 3)))
  (is (not= -5 (move-into-range -18 -10 -1 '(-5))))
  (is (not= -5 (move-into-range -17 -10 -1 '(-5)))))

(deftest test-is-in-range
  (is (true? (is-in-range 0 [0 0])))
  (is (true? (is-in-range 0 [0 1])))
  (is (false? (is-in-range 0 [-128 -1]))))

(deftest test-is-in-ranges
  (is (= '([0 0]) (is-in-ranges 0 [[0 0]])))
  (is (= nil (is-in-ranges 1 [[0 0]])))
  (is (= '([0 1] [0 1]) (is-in-ranges 1 [[0 1] [0 1]]))))

(deftest test-distance-to-range
  (is (= 1 (distance-to-range 1 [0 0])))
  (is (= 1 (distance-to-range -1 [-2 -2])))
  (is (= 1 (distance-to-range -1 [-5 -2]))))

(deftest test-closest-range
  (is (= [0 1] (closest-range 2 [[0 1] [4 5]])))
  (is (= [4 5] (closest-range 8 [[0 1] [4 5]])))
  (is (= [0 0] (closest-range 22 [[0 0]]))))

(deftest test-values-in-range?
  (is (true? (values-in-range? [0 0] [[0 0] [0 1]])))
  (is (true? (values-in-range? [-1 -1] [[-128 -128] [-1 -1]])))
  (is (true? (values-in-range? [0] [[0 5] [0 10]]))))

(deftest test-ranges-at-idx
  (is (= [[0 0]] (map (partial map #(nth %1 0)) [[[0] [0]]])))
  (is (= [[1 -1]] (map (partial map #(nth %1 1)) [[[0 1] [0 -1]]]))))

(defn bytes->short [arr]
  (-> arr
      (cond-> (not (bytes? arr)) (byte-array))
      (ByteBuffer/wrap)
      (.getShort)))

(defn short->bytes [short]
  (let [output (byte-array 2)]
    (-> output
        (ByteBuffer/wrap)
        (.putShort short))
    output))

(defn vectorized-move-bytes-into-range
  ([input min max] (vectorized-move-bytes-into-range input min max #{}))
  ([input min max skip]
   (let [ranges (split-number-line-min-max-into-bytewise-min-max min max short->bytes)
         skip-bytes (set (map short->bytes skip))]
     (-> input
         (byte-array)
         (map-into-ranges ranges skip-bytes)
         (bytes->short)))))

(deftest test-split-number-line-min-max-into-bytewise-min-max
  (let [vectorized #(->> (split-number-line-min-max-into-bytewise-min-max %1 %2 short->bytes)
                         (map (partial map vec))
                         (map vec)
                         (vec))]
    (is (= [[[0 0] [0 1]]] (vectorized 0 1)))
    (is (= [[[-1 -1] [-1 -1]] [[0 0] [0 1]]] (vectorized -1 1)))))

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

    (is (= -1 (make-short [-1 -1] -1 1 #{-5})))
    (is (= -1 (make-short [-1 -1] -10 1 #{-5})))
    (is (= -2 (make-short [-1 -2] -10 1 #{-5})))
    (is (= -10 (make-short [-1 -10] -10 1 #{-5})))
    (is (= -8 (make-short [-1 -11] -10 1 #{-5})))
    (is (= -6 (make-short [-1 -18] -10 1 #{-5})))
    (is (= -3 (make-short [-1 -15] -10 1 #{-5})))
    (is (= -4 (make-short [-1 -16] -10 1 #{-5})))
    (is (= -4 (make-short [-1 -17] -10 1 #{-5})))
    (is (= -6 (make-short [-1 -18] -10 1 #{-5})))
    (is (= -7 (make-short [-1 -19] -10 1 #{-5})))
    (is (= -4 (make-short [-1 -25] -10 1 #{-5})))))
