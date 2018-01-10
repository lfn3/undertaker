(ns net.lfn3.undertaker.byte-buffers
  (:require [net.lfn3.undertaker.bytes :as bytes])
  (:import (net.lfn3.undertaker OverrunException)))

(defn adjust-skip-indices [adjust-by skip-indices]
  (->> skip-indices
       (map (partial map #(- %1 adjust-by)))
       (filter (complement (partial every? neg?)))
       (map (partial map #(if (neg? %1) 0 %1)))))

(defn make-buffer [start-offset arrays skip-ranges]
  (if (= 0 start-offset)
    {::start-offset start-offset
     ::arrays       arrays
     ::skip-ranges  skip-ranges}
    (loop [to-skip start-offset
           arrays arrays]
      (let [arr-size (count (first arrays))]
        (if (<= arr-size to-skip)
          (recur (- to-skip arr-size)
                 (rest arrays))
          {::start-offset to-skip
           ::arrays       arrays
           ::skip-ranges  (adjust-skip-indices (- start-offset to-skip) skip-ranges)})))))

(defn get-bytes-and-apply-fn [number-of-bytes f buffer]
  (let [{:keys [::start-offset ::arrays ::skip-ranges]} buffer]
    (loop [idx start-offset
           arrays arrays
           skip skip-ranges
           found-bytes []]
      (if (= number-of-bytes (count found-bytes))
        (f found-bytes)
        (let [arr (first arrays)]
          (cond
            (and (< idx (count arr))
                 (not (bytes/is-in-ranges idx skip))) (recur (inc idx) arrays skip (conj found-bytes (aget arr idx)))
            (< idx (count arr)) (recur (inc idx) arrays skip found-bytes)
            (and (<= (count arr) idx) (seq (rest arrays))) (recur 0 (rest arrays) (adjust-skip-indices idx skip) found-bytes)
            :default (throw (OverrunException.))))))))

(defn get-short [buffer] (get-bytes-and-apply-fn 2 #(apply bytes/bytes->short %1) buffer))
(defn get-int [buffer] (get-bytes-and-apply-fn 4 #(apply bytes/bytes->int %1) buffer))