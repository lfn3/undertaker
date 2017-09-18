(ns undertaker.shrink
  (:require [undertaker.source.fixed :as fixed-source]
            [clojure.spec.alpha :as s]
            [undertaker.source :as source]
            [clojure.test.check.generators :as gen]
            [undertaker.proto :as proto]
            [undertaker.bytes :as bytes])
  (:import (com.lmax.undertaker OverrunException)))

(defn move-towards-0 [byte]
  (if (zero? byte)
    0
    (-> byte
        (bit-and 0xff)
        (dec)
        (unchecked-byte))))

(s/fdef move-towards-0
  :args (s/cat :byte ::bytes/byte)
  :ret ::bytes/byte
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [byte]} args]
          (or (= 0 ret)
              (< (bit-and 0xff ret)
                 (bit-and 0xff (:byte args)))))))

(defn snip-interval [bytes {:keys [::proto/interval-start ::proto/interval-end]}]
  (let [range (- interval-end interval-start)
        output (byte-array (- (count bytes) range))]
    (System/arraycopy bytes 0 output 0 interval-start)
    (System/arraycopy bytes (+ interval-start range) output interval-start (- (count bytes) interval-start range))
    output))

(defn is-overrun? [result-map]
  (or (instance? OverrunException (:undertaker.core/cause result-map))
      (->> result-map
           :undertaker.core/cause
           :actual
           (instance? OverrunException))
      (if (->> result-map
               :undertaker.core/cause
               (map :actual)
               (seq?))
        (->> result-map
             :undertaker.core/cause
             (map :actual)
             (filter (partial instance? OverrunException))
             (not-empty)
             (nil?)
             (not))
        false)))

(s/fdef is-overrun?
  :args (s/cat :result-map (s/keys :req [:undertaker.core/cause]))
  :ret boolean?)

(defn snip-intervals [bytes intervals fn]
  (loop [index 0
         intervals intervals
         bytes bytes]
    (if (not-empty intervals)
      (let [interval (nth intervals index)
            continue? (< (inc index) (count intervals))]
        (if-let [shrunk-bytes (snip-interval bytes interval)]
          (let [source (fixed-source/make-fixed-source shrunk-bytes)
                result (fn source)
                passed? (:undertaker.core/result result)
                overrun? (is-overrun? result)]
            (cond
              (and continue? (or overrun? passed?)) (recur (inc index)
                                                           intervals
                                                           bytes)
              ;TODO: figure out if I can optimize this a bit.
              (and continue? (not passed?) (not overrun?)) (recur 0 ;safest option is to restart, since we might have deleted a bunch of intervals.
                                                                  (source/get-intervals source)
                                                                  shrunk-bytes)
              (and (not continue?) (or overrun? passed?)) bytes
              (and (not continue?) (not overrun?) (not passed?)) shrunk-bytes))
          (when continue?
            (recur (inc index) intervals bytes))))
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

(s/fdef shrink-at!
  :args (s/with-gen (s/cat :bytes (s/and bytes?
                                         not-empty)
                           :index integer?)
                    #(gen/bind (s/gen (s/and bytes?
                                             not-empty))
                               (fn [byte-arr]
                                 (gen/tuple (gen/return byte-arr)
                                            (gen/choose 0 (dec (count byte-arr)))))))
  :ret bytes?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [bytes index]} args]
          (and (< index (count bytes))
               (= (count bytes)
                  (count ret))
               (>= (sum-abs bytes)
                   (sum-abs ret))))))

(defn move-bytes-towards-zero [bytes fn]
  (if-not (empty? bytes)
    (loop [last-failure-bytes bytes
           working-on 0
           shrunk-bytes (-> (byte-array bytes)              ;;clone it so we can mutate it safely.
                            (shrink-at! working-on))]
      (let [shrunk-source (fixed-source/make-fixed-source shrunk-bytes)
            keep-trying-current-byte? (not (zero? (nth shrunk-bytes working-on)))
            result-map (fn shrunk-source)
            passed? (true? (:undertaker.core/result result-map))
            work-on-next (if keep-trying-current-byte?
                           working-on
                           (inc working-on))
            continue? (< work-on-next (count shrunk-bytes))
            last-failure-bytes (if passed?
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
  ([bytes intervals f]
   (try
     (source/shrinking!)
     (let [shrunk-source (-> bytes
                             (snip-intervals intervals f)
                             (repeatedly-move-towards-zero f)
                             (fixed-source/make-fixed-source))
           _ (f shrunk-source)
           intervals (proto/get-intervals shrunk-source)
           shrunk-source (-> (proto/get-sourced-bytes shrunk-source)
                             (snip-intervals intervals f)
                             (fixed-source/make-fixed-source))]
       (f shrunk-source)                                    ;So we get the right intervals in place. TODO: remove this.
       shrunk-source)
     (finally (source/done-shrinking!)))))

(s/fdef shrink
  :args (s/cat :bytes bytes?
               :intervals (s/coll-of ::proto/interval)
               :fn fn?)
  :ret ::source/source)
