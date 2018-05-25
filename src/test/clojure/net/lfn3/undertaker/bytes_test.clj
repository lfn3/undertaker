(ns net.lfn3.undertaker.bytes-test
  (:require [clojure.test :refer [deftest is] :as t]
            [net.lfn3.undertaker.bytes :refer :all]
            [orchestra.spec.test :as orchestra.test]
            [net.lfn3.undertaker.specs.bytes]
            [net.lfn3.undertaker.test-utils :as test-utils]
            [net.lfn3.undertaker.core :as undertaker]
            [net.lfn3.undertaker.intervals :as intervals])
  (:import (java.nio ByteBuffer)))

(test-utils/use-standard-fixtures)

(test-utils/defchecks net.lfn3.undertaker.bytes
                      #{net.lfn3.undertaker.bytes/map-into-ranges!}) ;Issues with range generation.

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

(deftest test-bytes-are-in-range
  (is (true? (bytes-are-in-range [-2 115 -61 -12 -124 75 -10 -73] [[-1 -17 -1 -1 -1 -1 -1 -1] [-65 -16 0 0 0 0 0 0]]))))

(defn vectorized-move-bytes-into-range
  ([input min max] (vectorized-move-bytes-into-range input min max #{}))
  ([input min max skip]
   (let [ranges (split-number-line-min-max-into-bytewise-min-max min max short->bytes)
         skip-bytes (set (map short->bytes skip))
         ranges (punch-skip-values-out-of-ranges skip-bytes ranges)]
     (-> input
         (byte-array)
         (ByteBuffer/wrap)
         (map-into-ranges! ranges)
         (.getShort)))))

(deftest test-map-bytes-into-ranges-with-offset-buffer
  (let [map-into-vectorised-ranges! #(map-into-ranges! %1 (vec (map (comp vec (partial map byte-array)) %2)))]
    (is (= 4 (.getShort (map-into-vectorised-ranges! (ByteBuffer/wrap (byte-array [1 2 3 4]) 2 2) [[[0 0] [2 2]]]))))
    (is (= 2 (.getInt (map-into-vectorised-ranges! (ByteBuffer/wrap (byte-array [2 27 73 67 -38 97 58 -23 -58 -14]) 1 4)
                                        [[[0 0 0 0] [0 0 0 5]]]))))
    (is (= 285 (.getShort (map-into-vectorised-ranges! (ByteBuffer/wrap (byte-array [1 1])) [[[1 26] [1 26]] [[1 28] [4 0]]]))))
    (is (= -1.3236780977631986E301
           (-> [-2 115 -61 -12 -124 75 -10 -73]
               (byte-array)
               (ByteBuffer/wrap)
               (map-into-vectorised-ranges! [[[-1 -17 -1 -1 -1 -1 -1 -1] [-65 -16 0 0 0 0 0 0]]
                                  [[0 0 0 0 0 0 0 0] [127 -17 -1 -1 -1 -1 -1 -1]]])
               (vector)
               (buffers->bytes)
               (#(apply bytes->double %)))))
    (is (< (-> [-65 -23 -17 -116 -114 2 -55 55]
               (byte-array)
               (ByteBuffer/wrap)
               (map-into-vectorised-ranges! [[[-65 -16 0 0 0 0 0 0] [-1 -17 -1 -1 -1 -1 -1 -1]]
                                  [[0 0 0 0 0 0 0 0] [127 -17 -1 -1 -1 -1 -1 -1]]])
               (vector)
               (buffers->bytes)
               (#(apply bytes->double %)))
           -1.0))
    (is (-> [-65 -21 -111 16 -76 -16 -80 66]
            (byte-array)
            (ByteBuffer/wrap)
            (map-into-vectorised-ranges! [[[-65 -16 0 0 0 0 0 0] [-65 -32 0 0 0 0 0 0]]])
            (vector)
            (buffers->bytes)
            (vec)))
    (is (-> [-65 -21 -111 16 -76 -16 -80 66]
            (byte-array)
            (ByteBuffer/wrap)
            (map-into-vectorised-ranges! [[[-65 -16 0 0 0 0 0 0] [-65 -32 0 0 0 0 0 0]]])
            (vector)
            (buffers->bytes)
            (vec)))
    (is (-> [63 -19 10 -97 -115 -111 -66 -71]
            (byte-array)
            (ByteBuffer/wrap)
            (map-into-vectorised-ranges! [[[63 -16 0 0 0 0 0 0] [63 -32 0 0 0 0 0 0]]])
            (vector)
            (buffers->bytes)
            (vec)))
    (is (-> [63 -19 10 -97 -115 -111 -66 -71]
            (byte-array)
            (ByteBuffer/wrap)
            (map-into-vectorised-ranges! [[[63 -16 0 0 0 0 0 0] [63 -32 0 0 0 0 0 0]]])
            (vector)
            (buffers->bytes)
            (vec)))))

(deftest test-bound-range-to
  (is (= (bound-range-to 0 1 [[1 28] [4 0]]) [[1 28] [1 -1]])))

(deftest test-buffers->bytes
  (let [arr (byte-array [1 0 0 0 2 1 0 0 0 4])
        buffers [(ByteBuffer/wrap arr 0 1) (ByteBuffer/wrap arr 1 4) (ByteBuffer/wrap arr 5 1) (ByteBuffer/wrap arr 6 4)]]
    (is (= [1] (vec (buffers->bytes buffers 0 1))) (nth buffers 0))
    (is (= [0 0 0 2] (vec (buffers->bytes buffers 1 2))) (nth buffers 1))
    (is (= [1] (vec (buffers->bytes buffers 2 3))) (nth buffers 2))
    (is (= [0 0 0 4] (vec (buffers->bytes buffers 3 4)))) (nth buffers 3)))

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

(deftest test-punch-skip-values-out-of-range
  (let [vectorised-psvr #(intervals/printable-ranges (punch-skip-values-out-of-ranges %1 %2))]
    (is (= (vectorised-psvr [[1]] [[[1] [4]]]) [[[2] [4]]]))
    (is (= (vectorised-psvr [[2]] [[[1] [4]]]) [[[1] [1]] [[3] [4]]]))
    (is (= (vectorised-psvr [[2]] [[[4] [80]]]) [[[4] [80]]]))
    (is (= (vectorised-psvr [[0]] [[[0 0] [2 2]]]) [[[1 0] [2 2]]]))
    (is (= (vectorised-psvr [[0]] [[[0 1] [2 2]]]) [[[1 0] [2 2]]]))
    (is (= (vectorised-psvr [[0 0]] [[[0 0 0] [2 2 2]]]) [[[0 1 0] [2 2 2]]]))
    (is (= (vectorised-psvr [[1 0]] [[[0 0 0] [2 2 2]]]) [[[0 0 0] [0 -1 -1]] [[1 1 0] [2 2 2]]]))
    (is (= (vectorised-psvr [[0 0 0 3]] [[[0 0 0 1] [0 0 0 3]]]) [[[0 0 0 1] [0 0 0 2]]]))

    (is (= (vectorised-psvr [[-2]] [[[-4] [-1]]]) [[[-4] [-3]] [[-1] [-1]]]))
    (is (= (vectorised-psvr [[-128]] [[[-128] [-1]]]) [[[-127] [-1]]]))
    (is (= (vectorised-psvr [[-128]] [[[-128 -128] [-1 -1]]]) [[[-127 -128] [-1 -1]]]))
    (is (= (vectorised-psvr [[-1]] [[[-128 -128] [-1 -1]]]) [[[-128 -128] [-2 -1]]]))
    (is (= (vectorised-psvr [[-2 -1]] [[[-128 -128] [-1 -1]]]) [[[-128 -128] [-2 -2]] [[-1 0] [-1 -1]]]))

    (is (= (vectorised-psvr [[127]] [[[0] [127]]]) [[[0] [126]]]))
    (is (= (vectorised-psvr [[127]] [[[0 0] [127 0]]]) [[[0 0] [126 -1]]]))
    (is (= (vectorised-psvr [[-128]] [[[-128] [-1]]]) [[[-127] [-1]]]))

    (is (= (vectorised-psvr [[0 127]] [[[0 0] [4 0]]]) [[[0 0] [0 126]] [[0 -128] [4 0]]]))
    (is (= (vectorised-psvr [[0 -128]] [[[0 0] [0 -128]]]) [[[0 0] [0 127]]]))
    (is (= (vectorised-psvr [[4 2] [4 1]] [[[4 0] [19 0]]]) [[[4 0] [4 0]] [[4 3] [19 0]]]))
    (is (= (vectorised-psvr [[9 -50] [10 9]] [[[9 9] [10 9]]]) [[[9 9] [9 -51]] [[9 -49] [10 8]]]))))

(deftest test-collapse-identical-ranges
  (is (= [[[9 9] [10 8]]] (collapse-identical-ranges [[[9 9] [10 8]]]))))

(deftest test-map-into-ranges
  (is (= 0 (vectorized-move-bytes-into-range [0 0] 0 1)))
  (is (= 127 (vectorized-move-bytes-into-range [0 127] 0 127)))
  (is (= 5 (vectorized-move-bytes-into-range [0 0] 5 10)))
  (is (= 6 (vectorized-move-bytes-into-range [0 1] 5 10)))
  (is (= 7 (vectorized-move-bytes-into-range [0 2] 5 10)))
  (is (= 8 (vectorized-move-bytes-into-range [0 3] 5 10)))
  (is (= 9 (vectorized-move-bytes-into-range [0 4] 5 10)))
  (is (= 5 (vectorized-move-bytes-into-range [0 5] 5 10)))
  (is (= 6 (vectorized-move-bytes-into-range [0 6] 5 10)))
  (is (= 6 (vectorized-move-bytes-into-range [22 6] 5 10)))

  (is (= 258 (vectorized-move-bytes-into-range [1 2] 257 514)))
  (is (= 257 (vectorized-move-bytes-into-range [1 0] 257 514)))
  (is (= 257 (vectorized-move-bytes-into-range [1 1] 257 514)))
  (is (= 259 (vectorized-move-bytes-into-range [1 3] 257 514)))
  (is (= 510 (vectorized-move-bytes-into-range [1 -2] 257 514)))

  (is (= 0 (vectorized-move-bytes-into-range [0 0] -1 1)))
  (is (= 1 (vectorized-move-bytes-into-range [0 1] -1 1)))
  (is (= 0 (vectorized-move-bytes-into-range [0 2] -1 1)))
  (is (= -1 (vectorized-move-bytes-into-range [-1 -1] -1 1)))
  (is (= -1 (vectorized-move-bytes-into-range [-2 -2] -1 1)))
  (is (= -1 (vectorized-move-bytes-into-range [-2 -3] -1 1)))

  (is (= -1 (vectorized-move-bytes-into-range [-1 -1] -10 1)))
  (is (= -2 (vectorized-move-bytes-into-range [-1 -2] -10 1)))
  (is (= -10 (vectorized-move-bytes-into-range [-1 -10] -10 1)))
  (is (= -5 (vectorized-move-bytes-into-range [-1 -11] -10 1)))
  (is (= -10 (vectorized-move-bytes-into-range [-1 -16] -10 1)))
  (is (= -1 (vectorized-move-bytes-into-range [-1 -17] -10 1)))
  (is (= -10 (vectorized-move-bytes-into-range [-1 -26] -10 1)))

  (is (not= -5 (vectorized-move-bytes-into-range [-1 -1] -1 1 #{-5})))
  (is (not= -5 (vectorized-move-bytes-into-range [-1 -1] -10 1 #{-5})))
  (is (not= -5 (vectorized-move-bytes-into-range [-1 -2] -10 1 #{-5})))
  (is (not= -5 (vectorized-move-bytes-into-range [-1 -10] -10 1 #{-5})))
  (is (not= -5 (vectorized-move-bytes-into-range [-1 -11] -10 1 #{-5})))
  (is (not= -5 (vectorized-move-bytes-into-range [-1 -18] -10 1 #{-5})))
  (is (not= -5 (vectorized-move-bytes-into-range [-1 -15] -10 1 #{-5})))
  (is (not= -5 (vectorized-move-bytes-into-range [-1 -16] -10 1 #{-5})))
  (is (not= -5 (vectorized-move-bytes-into-range [-1 -17] -10 1 #{-5})))
  (is (not= -5 (vectorized-move-bytes-into-range [-1 -18] -10 1 #{-5})))
  (is (not= -5 (vectorized-move-bytes-into-range [-1 -19] -10 1 #{-5})))
  (is (not= -5 (vectorized-move-bytes-into-range [-1 -25] -10 1 #{-5}))))

(undertaker/defprop should-punch-out-values {}
  (let [upper (undertaker/short 1)
        lower (undertaker/short 0 (dec upper))
        skip (undertaker/set-of (partial undertaker/short lower upper))
        result (punch-skip-values-out-of-ranges (map short->bytes skip)
                                                (split-number-line-min-max-into-bytewise-min-max lower upper short->bytes))
        ranges-as-shorts (map (partial map #(apply bytes->short %)) result)
        length-of-ranges (->> (mapcat range (map first ranges-as-shorts) (map (comp inc last) ranges-as-shorts))
                              (count))
        length-of-initial-range (count (range lower (inc upper)))]
    (is (= length-of-ranges (- length-of-initial-range (count skip))))))