(ns undertaker.byte-buffers-test
  (:require [clojure.test :refer :all]
            [undertaker.core :as undertaker])
  (:import (com.lmax.undertaker ChainedByteBuffer)
           (java.nio ByteBuffer)))

(defn get-ops []
  (undertaker/vec-of (partial undertaker/from #{(let [b (undertaker/byte)]
                                                  [:put b #(.put %1 b)]) ;TODO: generate a bunch of these, loop through them.
                                                [:mark #(.mark %1)]
                                                [:reset #(.reset %1)]
                                                [:get #(.get %1)]
                                                [:array #(vec (.array %1))]
                                                [:limit #(.limit %1)]})))

(undertaker/defprop chained-buffer-wrapping-single-byte-buffer-should-behave-the-same {}
  (let [some-bytes (undertaker/vec-of undertaker/byte)
        underlying (byte-array some-bytes)
        duplicated (byte-array some-bytes)
        byte-buffer (ByteBuffer/wrap underlying)
        chained-buffer (ChainedByteBuffer. (into-array ByteBuffer [(ByteBuffer/wrap duplicated)]))]

    (loop [remaining-ops (get-ops)]
      (when-let [op (first remaining-ops)]
        (let [ex1 (atom nil)
              ex2 (atom nil)
              result1 (atom nil)
              result2 (atom nil)]
          (try
            (reset! result1 ((last op) chained-buffer))
            (catch Exception e
              (reset! ex1 e)))
          (try
            (reset! result2 ((last op) byte-buffer))
            (catch Exception e
              (reset! ex2 e)))

          (when (and (nil? @ex1) (nil? @ex2))
            (if (= (type @result1) ChainedByteBuffer)
              (is (instance? ByteBuffer @result2))
              (is (= @result1 @result2)))

            (is (= (vec underlying) (vec duplicated)))

            (recur (rest remaining-ops))))))))

(undertaker/defprop chained-buffer-wrapping-split-byte-buffer-should-behave-the-same {}
  (let [some-bytes (undertaker/vec-of undertaker/byte)
        split-at (undertaker/int 0 (count some-bytes))
        underlying (byte-array some-bytes)
        first-duplicated (byte-array (take split-at some-bytes))
        second-duplicated (byte-array (drop split-at some-bytes))
        byte-buffer (ByteBuffer/wrap underlying)
        chained-buffer (ChainedByteBuffer. (into-array ByteBuffer [(ByteBuffer/wrap first-duplicated)
                                                                   (ByteBuffer/wrap second-duplicated)]))]

    (loop [remaining-ops (get-ops)]
      (when-let [op (first remaining-ops)]
        (let [ex1 (atom nil)
              ex2 (atom nil)
              result1 (atom nil)
              result2 (atom nil)]
          (try
            (reset! result1 ((last op) chained-buffer))
            (catch Exception e
              (reset! ex1 e)))
          (try
            (reset! result2 ((last op) byte-buffer))
            (catch Exception e
              (reset! ex2 e)))

          (when (and (nil? @ex1) (nil? @ex2))
            (if (= (type @result1) ChainedByteBuffer)
              (is (instance? ByteBuffer @result2))
              (is (= @result1 @result2)))

            (is (= (vec underlying) (concat (vec first-duplicated) (vec second-duplicated))))

            (recur (rest remaining-ops))))))))
