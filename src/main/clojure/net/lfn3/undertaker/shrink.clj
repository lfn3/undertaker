(ns net.lfn3.undertaker.shrink
  (:require [net.lfn3.undertaker.source.fixed :as fixed-source]
            [net.lfn3.undertaker.source :as source]
            [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.intervals :as intervals])
  (:import (net.lfn3.undertaker OverrunException UniqueInputValuesExhaustedException)))

(defn move-towards-0 [byte]
  (if (zero? byte)
    0
    (-> byte
        (bit-and 0xff)
        (dec)
        (unchecked-byte))))

(defn snip-interval [^bytes bytes {:keys [::proto/interval-start ::proto/interval-end]}]
  (let [range (- interval-end interval-start)
        output (byte-array (- (count bytes) range))]
    (System/arraycopy bytes 0 output 0 interval-start)
    (System/arraycopy bytes (+ interval-start range) output interval-start (- (count bytes) interval-start range))
    output))

(defn ignoreable-ex? [ex]
  (or (instance? OverrunException ex)
      (instance? UniqueInputValuesExhaustedException ex)))

(defn is-overrun? [result-map]
  (let [cause (:net.lfn3.undertaker.core/cause result-map)]
    (or (ignoreable-ex? cause)
        (->> cause
             :actual
             (ignoreable-ex?))
        (if (->> cause
                 (seq?))
          (->> cause
               (map :actual)
               (filter ignoreable-ex?)
               (not-empty)
               (nil?)
               (not))
          false))))

;This relies on the fact that snippable intervals are direct at the moment.
(defn is-snippable? [interval]
  (->> interval
       ::proto/hints
       (some (comp (partial = ::proto/snippable) first))
       (nil?)
       (not)))

(defn snip-intervals [bytes intervals fn]
  (loop [index 0
         intervals (filter is-snippable? intervals)
         bytes bytes]
    (if (not-empty intervals)
      (let [interval (nth intervals index)
            continue? (< (inc index) (count intervals))
            shrunk-bytes (snip-interval bytes interval)
            source (fixed-source/make-fixed-source shrunk-bytes)
            _ (source/reset source)
            result (fn source)
            passed? (:net.lfn3.undertaker.core/result result)
            overrun? (is-overrun? result)]
        (cond
          (and continue? (or overrun? passed?)) (recur (inc index)
                                                       intervals
                                                       bytes)
          ;TODO: figure out if I can optimize this a bit.
          (and continue? (not passed?) (not overrun?)) (recur 0 ;safest option is to restart, since we might have deleted a bunch of intervals.
                                                              (filter is-snippable? (source/get-intervals source))
                                                              shrunk-bytes)
          (and (not continue?) (or overrun? passed?)) bytes
          (and (not continue?) (not overrun?) (not passed?)) shrunk-bytes))
      bytes)))

(defn shrink-at!
  "MUTATES!"
  ([bytes idx]
   (aset-byte bytes idx (move-towards-0 (aget bytes idx)))
   bytes))

(defn sum-abs [coll]
  (->> coll
       (map (partial bit-and 0xff))
       (reduce +)))

(defn move-bytes-towards-zero [bytes fn]
  (if-not (empty? bytes)
    (loop [last-failure-bytes bytes
           working-on 0
           shrunk-bytes (-> (byte-array bytes)              ;;clone it so we can mutate it safely.
                            (shrink-at! working-on))]
      (let [shrunk-source (fixed-source/make-fixed-source shrunk-bytes)
            _ (source/reset shrunk-source)
            keep-trying-current-byte? (not (zero? (nth shrunk-bytes working-on)))
            result-map (fn shrunk-source)
            passed? (true? (:net.lfn3.undertaker.core/result result-map))
            overrun? (is-overrun? result-map)
            work-on-next (if keep-trying-current-byte?
                           working-on
                           (inc working-on))
            continue? (< work-on-next (count shrunk-bytes))
            last-failure-bytes (if (or passed? overrun?)
                                 last-failure-bytes
                                 (byte-array shrunk-bytes))] ;Defensive clone
        (when (and (not keep-trying-current-byte?) continue?) ;If we're about to move on, put the last failing byte back in.
          (aset-byte shrunk-bytes working-on (aget last-failure-bytes working-on)))
        (if continue?
          (recur last-failure-bytes work-on-next (shrink-at! shrunk-bytes work-on-next))
          last-failure-bytes)))
    bytes))

(defn repeatedly-move-towards-zero [bytes fn]
  (loop [sum (sum-abs bytes)
         bytes bytes]
    (let [shrunk (move-bytes-towards-zero bytes fn)
          after-shrink-sum (sum-abs bytes)]
      (if-not (= sum after-shrink-sum)
        (recur after-shrink-sum shrunk)
        shrunk))))

(defn shrink
  ([source f]
   (try
     (let [bytes (source/get-sourced-bytes source)
           intervals (source/get-intervals source)]
       (source/shrinking!)
       (let [shrunk-source (-> bytes
                               (snip-intervals intervals f)
                               (repeatedly-move-towards-zero f)
                               (fixed-source/make-fixed-source))
             _ (source/reset shrunk-source)
             _ (f shrunk-source)
             intervals (source/get-intervals shrunk-source)
             shrunk-source (-> (source/get-sourced-bytes shrunk-source)
                               (snip-intervals intervals f)
                               (fixed-source/make-fixed-source))
             _ (source/reset shrunk-source)
             result-map (f shrunk-source)]
         (source/add-source-data-to-results-map shrunk-source result-map)))
     (finally (source/done-shrinking!)))))
