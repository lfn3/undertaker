(ns net.lfn3.undertaker.byte-buffers
  (:require [clojure.test :refer [deftest is] :as t]
            [net.lfn3.undertaker.core :as undertaker]
            [net.lfn3.undertaker.byte-buffers :as buffers])
  (:import (java.nio ByteBuffer)))

(undertaker/defprop should-behave-the-same-as-byte-buffer-in-simple-case {}
  (let [arr (byte-array (undertaker/vec-of undertaker/byte 2 16))
        pos (undertaker/int 0 (- (count arr) 2))
        u-buf (buffers/make-buffer pos [arr] [])
        j-buf (ByteBuffer/wrap arr)]
    (is (= (buffers/get-short u-buf) (.getShort j-buf pos)))))
