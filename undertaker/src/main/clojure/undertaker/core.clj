(ns undertaker.core
  (:gen-class)
  (:refer-clojure :exclude [int byte long])
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [clojure.string :as str]
            [clojure.test :as t]
            [undertaker.proto :as proto]
            [undertaker.source :as source]
            [undertaker.source.wrapped-random :as wrapped-random-source]
            [undertaker.source.fixed :as fixed-source]
            [clojure.test.check.generators :as gen]
            [undertaker.util :as util])
  (:import (java.util Random Arrays)
           (java.nio ByteBuffer)
           (com.lmax.undertaker OverrunException)))

(defonce seed-uniquifier* (volatile! (clojure.core/long 8682522807148012)))

(defn seed-uniquifier []
  (vswap! seed-uniquifier* #(unchecked-multiply (clojure.core/long %1) ;TODO: get rid of casts.
                                                (clojure.core/long 181783497276652981))))

(defn next-seed [seed]
  (bit-xor (seed-uniquifier) (inc seed)))

(s/fdef next-seed
  :args (s/cat :seed integer?)
  :ret integer?)

(def ^:dynamic *source* nil)

(defn move-into-range
  ([byte min max]
   (move-into-range byte min max Byte/MIN_VALUE Byte/MAX_VALUE))
  ([number min max type-min type-max]
   (let [type-range (- type-max type-min)
         range (- max min)
         fixed-offset (- type-min)]                         ;Due to everything in java being signed.
     (if (= range 0)
       min
       (let [divisor (/ type-range range)
             adjusted-number (+ fixed-offset number)]
         (Math/round (double (+ min (/ adjusted-number divisor)))))))))

(s/fdef move-into-range
  :args (s/alt
          :byte (s/cat :byte ::util/byte
                       :min ::util/byte
                       :max ::util/byte)
          :integer (s/cat :integer integer?
                          :min integer?
                          :max integer?
                          :type-min integer?
                          :type-max integer?))
  :ret integer?
  :fn (fn [{:keys [args ret]}]
        (let [inner-args (last args)]
          (and (<= (:min inner-args) ret)
               (>= (:max inner-args) ret)))))

(defn format-interval-name [name & args]
  (str name " [" (str/join " " args) "]"))

(defmacro with-interval [name & body]
  `(let [interval-token# (source/push-interval *source* ~name)
         result# (do ~@body)]
     (source/pop-interval *source* interval-token# result#)
     result#))

(defn move-towards-0 [byte]
  (if (zero? byte)
    0
    (-> byte
        (bit-and 0xff)
        (dec)
        (unchecked-byte))))

(s/fdef move-towards-0
  :args (s/cat :byte ::util/byte)
  :ret ::util/byte
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

(defn snip-intervals [bytes intervals fn]
  (loop [index 0
         intervals intervals
         bytes bytes]
    (if (not-empty intervals)
      (let [interval (nth intervals index)
            shrunk-bytes (snip-interval bytes interval)
            source (fixed-source/make-fixed-source shrunk-bytes)
            result (fn source)
            passed? (::result result)
            overrun? (instance? OverrunException (::cause result))
            continue? (< (inc index) (count intervals))]
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
      bytes)))

(defn shrink-at!
  "MUTATES!"
  ([bytes idx]
   (aset-byte bytes idx (move-towards-0 (aget bytes idx)))
   bytes))

(defn sum-abs [coll]
  (->> coll
       (map util/abs)
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
            passed? (true? (::result result-map))
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

(defn shrink
  ([bytes intervals f]
   (source/shrinking!)
   (let [shrunk-source (-> bytes
                           (snip-intervals intervals f)
                           (move-bytes-towards-zero f)
                           (fixed-source/make-fixed-source))]
     (f shrunk-source)                                      ;So we get the right intervals in place. TODO: remove this.
     (source/done-shrinking!)
     shrunk-source)))

(s/fdef shrink
  :args (s/cat :bytes bytes?
               :intervals (s/coll-of ::proto/interval)
               :fn fn?)
  :ret ::source/source)

(defn ^String already-bound-source-error-string []
  (str
    "The *source* var has already been set, and something is trying to bind another value to it.
This probably means you've nested tests inside each other.

If you can't find the cause of the error, please raise an issue at "
    util/bug-tracker-url))

(defn failure? [test-report]
  (-> test-report
      :type
      (= :fail)))

(defn get-failures-from-test-reports [test-reports]
  (->> test-reports
       (filter failure?)
       (seq)))

(defn check-test-reports [test-reports]
  (not-any? failure? test-reports))

(defn make-report-fn [an-atom]
  (fn [msg]
    (swap! an-atom conj msg)))

(defn wrap-fn [f]
  (fn [source]
    (let [result (atom [])
          report-fn (make-report-fn result)]
      (when (not (nil? *source*))
        (throw (IllegalStateException. (already-bound-source-error-string))))
      (with-bindings {#'t/report report-fn
                      #'*source* source}
        (try
          (f)
          {::result   (check-test-reports @result)
           ::cause    (get-failures-from-test-reports @result)
           ::reported @result}
          (catch Throwable e
            {::result   false
             ::cause    e
             ::reported @result})
          (finally
            (reset! result [])))))))

(defn run-prop-1 [source f]
  (let [f (wrap-fn f)
        result-map (-> (f source)
                       (assoc ::generated-values (map ::proto/generated-value
                                                      (->> source
                                                           (source/get-intervals)
                                                           (filter (comp nil? ::proto/interval-parent-id))))))]
    (if (::result result-map)
      result-map
      (let [shrunk-source (shrink (source/get-sourced-bytes source) (source/get-intervals source) f)]
        (assoc result-map ::shrunk-values (->> (source/get-intervals shrunk-source)
                                               (filter (comp nil? ::proto/interval-parent-id))
                                               (map ::proto/generated-value)))))))

(s/def ::result boolean?)
(s/def ::generated-values any?)
(s/def ::shrunk-values any?)
(s/def ::seed integer?)
(s/def ::results-map (s/keys :req [::result ::generated-values]
                             :opt [::shrunk-values ::seed]))

(s/def ::prop-fn (s/fspec :args (s/cat :source ::source/source)
                          :ret boolean?))

(s/fdef run-prop-1
  :args (s/cat :source ::source/source
               :fn fn?)
  :ret ::results-map)

(defn run-prop
  ([{:keys [::seed ::iterations]
     :or   {seed       (bit-xor (System/nanoTime) (seed-uniquifier))
            iterations 1000}
     :as   opts-map}
    f]
   (let [source (wrapped-random-source/make-source seed)
         result (loop [iterations-left iterations]
                  (let [run-data (run-prop-1 source f)]
                    (if (and (-> run-data
                                 ::result
                                 (true?))
                             (> iterations-left 1)
                             (source/used? source))         ;If a source is unused, there isn't much point in rerunning
                      (do                                   ;the test, since nothing will change
                        (source/reset source)
                        (recur (dec iterations-left)))
                      (-> run-data
                          (assoc ::source-used (source/used? source))
                          (assoc ::iterations-run (- iterations (dec iterations-left)))
                          (cond->
                            seed (assoc ::seed seed))))))]
     (source/done-with-test!)
     result)))

(s/def ::iterations integer?)
(s/def ::prop-opts-map (s/keys :opt [::seed ::iterations]))

(s/fdef run-prop
  :args (s/cat :opts-map ::prop-opts-map
               :fn fn?)
  :ret ::results-map)

;Mapping straight to bytes doesn't work since the repr of an int is laid out differently.
;i.e. int max is 127 -1 -1 -1, int min is -128 0 0 0.
;the range of allowed bytes in this case is actually 127 127 127 127 -> -128 -128 -128 -128
;can special case max and min, but I still have to deal with larger ints.
;i.e. 2027483647 -> 120 -40 -15 -1, -2027483647 -> -121 39 14 1, we'd want to generate 119 40 ... (1999113729),
;so just working off the range wouldn't work.

; -1 -> +1 = [-1 -1 -1 -1] -> [0 0 0 1]
; generate first byte. If it's -1 then the rest of the options are fixed?
; not quite - there's the zero case.
; can split into -ve and

;; This is here for Rich-ean posterity: how I arrived at the fn below, and commentary explaining bits.
;(if (neg? first-genned)                              ;We know it's going to be in the negative part of the range (and that that part of the range exists)
;  (if (= first-genned (first mins))                  ;Only generate up to the limit
;    )
;  (if (= first-genned (first maxes))                 ;If first genned = max then we can only generate up to the limit.
;    (source/get-byte source
;                     (min 0 (nth maxes 2))           ;Max might be -ve (> 127)
;                     (if (neg? (nth maxes 2))
;                       Byte/MAX_VALUE
;                       (nth maxes 2)))
;    ()))

(defn generate-next-byte-for-int [source idx all-maxes? mins maxes]
  (let [floor (nth mins idx)
        ceiling (nth maxes idx)
        flip? (= 1 (Integer/compareUnsigned floor ceiling))
        [floor ceiling] (if flip? [ceiling floor] [floor ceiling])] ;;i.e. -1 > -2
    (if all-maxes? (source/get-byte source floor ceiling) (source/get-byte source))))

(defn is-max? [val idx mins maxes]
  (let [target-array (if (neg? val)
                       mins
                       maxes)]
    (= val (aget target-array idx))))

(defn bytes->int [arr]
  (-> arr
      (cond-> (not (bytes? arr)) (byte-array))
      (ByteBuffer/wrap)
      (.getInt)))

(defn bytes->long [arr]
  (-> arr
      (cond-> (not (bytes? arr)) (byte-array))
      (ByteBuffer/wrap)
      (.getLong)))

(defn format-failed [name results]
  (format "%s failed after running %d times.

The simplest values we could make the test fail with were:
%s

The initial failing values were:
%s

The seed that generated the initial case was %s.
If you want to rerun this particular failing case, you can add this seed to the test.

If you're using Clojure, you can add :undertaker.core/seed to this test's options map:
(defprop %s {:undertaker.core/seed %s} ...)

If you're using Java and jUnit, you can add an annotation to the test:
@Test
@com.lmax.undertaker.junit.Seed(%s)
public void %s() { ... }"
          name
          (::iterations-run results)
          (vec (::shrunk-values results))
          (vec (::generated-values results))
          (::seed results)
          name
          (::seed results)
          name
          (::seed results)))

(defn format-not-property-test-failed [name results]
  (format "This test did not contain any calls to undertaker generators, so was not treated as a property test and repeatedly run or shrunk."
          name))

(defn format-not-property-passed [name results]
  (format "%s did not contain any calls to undertaker generators, and so was not treated as a property test and run repeatedly.
You probably want to replace (defprop %s { opts... } test-body...) with (deftest %s test-body...)"
          name
          name
          name))

(defn format-results [name results]
  (cond
    (and (not (::source-used results)) (::result results)) (format-not-property-passed name results)
    (not (::source-used results)) (format-not-property-test-failed name results)
    (not (::result results)) (format-failed name results)
    :default nil))

(s/fdef format-results
  :args (s/cat :results ::results-map)
  :ret string?)

;; === === === === === === === ===
;; Public api
;; === === === === === === === ===

(defn bool
  ([]
   (with-interval (format-interval-name "bool")
     (if (= 1 (source/get-byte *source* 0 1))
       true
       false))))

(s/fdef bool
  :args (s/cat)
  :ret boolean?)

(defn byte
  ([] (byte Byte/MIN_VALUE Byte/MAX_VALUE))
  ([min] (byte min Byte/MAX_VALUE))
  ([min max]
   (with-interval (format-interval-name "byte" min max)
     (source/get-byte *source* min max))))

(defn int
  ([] (int Integer/MIN_VALUE Integer/MAX_VALUE))
  ([min] (int min Integer/MAX_VALUE))
  ([floor ceiling]
   (with-interval (format-interval-name "int" floor ceiling)
     (let [mins (util/get-bytes-from-int floor)
           maxes (util/get-bytes-from-int ceiling)
           first-genned (source/get-byte *source* (first mins) (first maxes))
           output-arr (byte-array 4)
           negative? (neg? first-genned)
           maxes (if (and negative? (not (neg? ceiling)))
                   (util/get-bytes-from-int (min -1 ceiling)) ;If we've already generated a -ve number, then the max is actually -1
                   maxes)
           mins (if (and (not negative?) (neg? floor))
                  (util/get-bytes-from-int (max 0 floor))   ;Conversely, if we've generated a +ve number, then the minimum is now zero.
                  mins)]
       (aset output-arr 0 first-genned)
       (loop [idx 1
              all-maxes? (is-max? first-genned 0 mins maxes)]
         (let [next-val (generate-next-byte-for-int *source* idx all-maxes? mins maxes)]
           (aset output-arr idx next-val)
           (when (< (inc idx) (count output-arr))
             (recur (inc idx)
                    (and all-maxes? (is-max? next-val idx mins maxes))))))
       (bytes->int output-arr)))))

(s/fdef int
  :args (s/cat :min (s/? int?)
               :max (s/? int?))
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Integer/MIN_VALUE
                      max Integer/MAX_VALUE}} args]
          (and                                              ;(<= min max)
            (<= min ret)
            (>= max ret)))))

(defn long
  ([] (long Long/MIN_VALUE Long/MAX_VALUE))
  ([min] (long min Long/MAX_VALUE))
  ([floor ceiling]
   (with-interval (format-interval-name "long" floor ceiling)
     (let [mins (util/get-bytes-from-long floor)
           maxes (util/get-bytes-from-long ceiling)
           first-genned (source/get-byte *source* (first mins) (first maxes))
           output-arr (byte-array 8)
           negative? (neg? first-genned)
           maxes (if (and negative? (not (neg? ceiling)))
                   (util/get-bytes-from-long (min -1 ceiling)) ;If we've already generated a -ve number, then the max is actually -1
                   maxes)
           mins (if (and (not negative?) (neg? floor))
                  (util/get-bytes-from-long (max 0 floor))  ;Conversely, if we've generated a +ve number, then the minimum is now zero.
                  mins)]
       (aset output-arr 0 first-genned)
       (loop [idx 1
              all-maxes? (is-max? first-genned 0 mins maxes)]
         (let [next-val (generate-next-byte-for-int *source* idx all-maxes? mins maxes)]
           (aset output-arr idx next-val)
           (when (< (inc idx) (count output-arr))
             (recur (inc idx)
                    (and all-maxes? (is-max? next-val idx mins maxes))))))
       (bytes->long output-arr)))))

(s/fdef long
  :args (s/cat :min (s/? int?)
               :max (s/? int?))
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Long/MIN_VALUE
                      max Long/MAX_VALUE}} args]
          (and                                              ;(<= min max)
            (<= min ret)
            (>= max ret)))))


(def default-max-size 64)

;TODO bias this so it's more likely to produce longer seqs.
(defn should-generate-elem? [source min max len]
  (with-interval (format-interval-name "should-generate-elem" min max len)
    (= 1 (cond
           (> min len) (source/get-byte source 1 1)
           (< max (inc len)) (source/get-byte source 0 0)
           :default (source/get-byte source 0 1)))))

(defn vec-of
  ([elem-gen] (vec-of elem-gen 0))
  ([elem-gen min] (vec-of elem-gen min default-max-size))
  ([elem-gen min max]
   (with-interval (format-interval-name "vec" min max)
     (loop [result []]
       (let [i (count result)]
         (if-let [next (with-interval (format-interval-name "chunk for vector" i)
                         (when-let [gen-next? (should-generate-elem? *source* min max i)]
                           (elem-gen)))]
           (recur (conj result next))
           result))))))

(defn from
  ([coll]
   (with-interval (format-interval-name "from" coll)
     (let [target-idx (int 0 (dec (count coll)))]
       (nth (vec coll) target-idx)))))

(s/fdef from
  :args (s/cat :coll (s/coll-of any?))
  :ret any?
  :fn (fn [{:keys [args ret]}] (contains? (set (:coll args)) ret)))

(def any-gens #{bool
                int})

(defn any
  ([] (any #{}))
  ([exclusions]
   (with-interval (format-interval-name "any")
     (let [chosen-generator (from (remove exclusions any-gens))]
       (chosen-generator)))))

(s/fdef any
  :args (s/cat :exclusions (s/or :fn (s/fspec :args (s/cat :item any?)
                                              :ret boolean?)
                                 :set set?))
  :ret any?)

(defmacro defprop [name opts & body]
  (let [name-string (str name)]
    (when-not (map? opts)
      (throw (IllegalArgumentException. "The second argument to defprop must be a map literal.")))
    `(t/deftest ~name
       (let [result# (run-prop ~opts (fn [] (do ~@body)))]
         (dorun (map t/report (::reported result#)))
         (when-let [message# (format-results ~name-string result#)]
           (println message#))))))
