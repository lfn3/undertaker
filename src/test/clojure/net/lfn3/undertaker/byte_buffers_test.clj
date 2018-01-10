(ns net.lfn3.undertaker.byte-buffers-test
  (:require [clojure.test :refer [deftest is] :as t]
            [net.lfn3.undertaker.core :as undertaker]
            [net.lfn3.undertaker.byte-buffers :as buffers])
  (:import (java.nio ByteBuffer)))

(undertaker/defprop should-behave-the-same-as-java-byte-buffer-for-shorts-without-skip {}
  (let [arr (byte-array (undertaker/vec-of undertaker/byte 2 16))
        pos (undertaker/int 0 (- (count arr) 2))
        u-buf (buffers/make-buffer pos [arr] [])
        j-buf (ByteBuffer/wrap arr)]
    (is (= (buffers/get-short u-buf) (.getShort j-buf pos)))))

(undertaker/defprop should-behave-the-same-as-java-byte-buffer-for-ints-without-skip {}
  (let [arr (byte-array (undertaker/vec-of undertaker/byte 4 16))
        pos (undertaker/int 0 (- (count arr) 4))
        u-buf (buffers/make-buffer pos [arr] [])
        j-buf (ByteBuffer/wrap arr)]
    (is (= (buffers/get-int u-buf) (.getInt j-buf pos)))))

(undertaker/defprop should-behave-the-same-as-java-byte-buffer-when-skipping-beginning {}
  (let [arr (byte-array (undertaker/vec-of undertaker/byte 4 16))
        pos (undertaker/int 0 (- (count arr) 4))
        u-buf (buffers/make-buffer 0 [arr] (if-not (zero? pos)
                                             [[0 (dec pos)]]
                                             []))
        j-buf (ByteBuffer/wrap arr)]
    (is (= (buffers/get-int u-buf) (.getInt j-buf pos)))))

(undertaker/defprop should-behave-the-same-as-java-byte-buffer-when-splitting-array {}
  (let [arr (byte-array (undertaker/vec-of undertaker/byte 4 16))
        pos (undertaker/int 0 (- (count arr) 4))
        split-pos (undertaker/int 0 (dec (count arr)))
        u-buf (buffers/make-buffer 0
                                   (map byte-array (split-at split-pos arr))
                                   (if-not (zero? pos)
                                     [[0 (dec pos)]]
                                     []))
        j-buf (ByteBuffer/wrap arr)]
    (is (= (buffers/get-int u-buf) (.getInt j-buf pos)))))

(deftest should-be-able-to-skip-bytes
  (let [arr (byte-array [0 0 0 0 1])
        buf (buffers/make-buffer 0 [arr] [[0 0]])]
    (is (= (buffers/get-int buf) 1)))
  (let [arr (byte-array [0 1 0 0 1])
        buf (buffers/make-buffer 0 [arr] [[1 1]])]
    (is (= (buffers/get-int buf) 1)))
  (let [arr (byte-array [1 0 0 0 1 1])
        buf (buffers/make-buffer 0 [arr] [[0 0] [4 4]])]
    (is (= (buffers/get-int buf) 1))))