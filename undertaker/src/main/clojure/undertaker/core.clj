(ns undertaker.core
  (:gen-class)
  (:refer-clojure :exclude [int byte long double])
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [clojure.string :as str]
            [clojure.test :as t]
            [undertaker.proto :as proto]
            [undertaker.source :as source]
            [undertaker.source.multi-source :as source.multi]
            [undertaker.source.fixed :as fixed-source]
            [clojure.test.check.generators :as gen]
            [undertaker.util :as util]
            [undertaker.shrink :as shrink])
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
         (Math/round (clojure.core/double (+ min (/ adjusted-number divisor)))))))))

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

(defn ^String already-bound-source-error-string []
  (str
    "The *source* var has already been set, and something is trying to bind another value to it.
This probably means you've nested tests inside each other.

If you can't find the cause of the error, please raise an issue at "
    util/bug-tracker-url))

(defn failure? [test-report]
  (-> test-report
      :type
      #{:fail :error}))

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
      (when-not (nil? *source*)
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
                       (assoc ::intervals (source/get-intervals source))
                       (assoc ::generated-bytes (vec (source/get-sourced-bytes source)))
                       (assoc ::generated-values (map ::proto/generated-value
                                                      (->> source
                                                           (source/get-intervals)
                                                           (filter (comp nil? ::proto/interval-parent-id))))))]
    (if (::result result-map)
      result-map
      (let [shrunk-source (shrink/shrink (source/get-sourced-bytes source) (source/get-intervals source) f)]
        (-> result-map
            (assoc ::shrunk-intervals (source/get-intervals shrunk-source))
            (assoc ::shrunk-bytes (vec (source/get-sourced-bytes shrunk-source)))
            (assoc ::shrunk-values (->> (source/get-intervals shrunk-source)
                                        (filter (comp nil? ::proto/interval-parent-id))
                                        (map ::proto/generated-value))))))))

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
  ([{:keys [:seed :iterations]
     :or   {seed       (bit-xor (System/nanoTime) (seed-uniquifier))
            iterations 1000}
     :as   opts-map}
    f]
   (let [source (source.multi/make-source seed)
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
(s/def ::prop-opts-map (s/keys :opt-un [::seed ::iterations]))

(s/fdef run-prop
  :args (s/cat :opts-map ::prop-opts-map
               :fn fn?)
  :ret ::results-map)

(defn potentially-matched-disallowed-values [bytes disallowed-values]
  (->> disallowed-values
       (filter #(= (inc (count bytes)) (count %1)))         ;Make sure we only check against values we're about to generate
       (filter #(map = (take (dec (count %1)) bytes)))))    ;Check if all bar the last byte match the disallowed value.
;If so we could potentially generate that as the next byte.
(s/fdef potentially-matched-disallowed-values
  :args (s/cat :bytes ::util/bytes :disallowed-values (s/coll-of ::util/bytes))
  :ret (s/coll-of ::util/bytes))

(defn next-byte-in-range? [floor ceiling value]
  (and (<= (bit-and 0xff floor) (bit-and 0xff value))
       (<= (bit-and 0xff value) (bit-and 0xff ceiling))))

(defn skip-disallowed-values
  [disallowed-values generated-byte]
  ;We have to do this repeatedly since we might have pushed the value up into another disallowed value.
  (loop [disallowed-bytes (map last disallowed-values)      ;We know that we must be dealing with the last part of the disallowed byte at this point
         last-altered-val generated-byte]
    (let [at-or-below-generated (filter #(util/unsigned<= %1 last-altered-val) disallowed-bytes)
          next-altered-val (->> at-or-below-generated
                                (count)
                                (+ last-altered-val)
                                (unchecked-byte))]
      (if (= next-altered-val last-altered-val)
        last-altered-val
        (recur (remove (set at-or-below-generated) disallowed-bytes)
               next-altered-val)))))

(s/fdef skip-disallowed-values
  :args (s/cat :disallowed-values (s/coll-of ::util/bytes) :generated-byte ::util/byte)
  :ret ::util/byte
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [disallowed-values]} args]
          (not-any? (partial = ret) (map last disallowed-values)))))

(defn generate-next-byte-for-double
  [source idx all-maxes? mins maxes disallowed-values]
  (if all-maxes?
    (let [floor (nth mins idx)
          ceiling (nth maxes idx)
          flip? (= -1 (Integer/compareUnsigned floor ceiling))
          [floor ceiling] (if flip? [ceiling floor] [floor ceiling]) ;;i.e. -1 > -2
          disallowed-values (if all-maxes?
                              (filter #(next-byte-in-range? floor ceiling (last %1)) disallowed-values)
                              disallowed-values)]
      (->> (util/unsigned-range->generator-range floor ceiling)
           (#(- %1 (count disallowed-values)))
           (source/get-ubyte source)
           (skip-disallowed-values disallowed-values)
           (util/map-generated-byte-into-unsigned-range floor ceiling)))
    (->> (- -1 (count disallowed-values))
         (source/get-ubyte source)
         (skip-disallowed-values disallowed-values))))

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
    (if all-maxes? (->> (util/signed-range->generator-range floor ceiling)
                        (source/get-ubyte *source*)
                        (util/map-generated-byte-into-signed-range floor ceiling))
                   (source/get-ubyte source))))

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

(defn bytes->double [arr]
  (-> arr
      (cond-> (not (bytes? arr)) (byte-array))
      (ByteBuffer/wrap)
      (.getDouble)))

(defn format-failed [name results]
  (format "%s failed after running %d times.

The simplest values we could make the test fail with were:
%s

The initial failing values were:
%s

The seed that generated the initial case was %s.
If you want to rerun this particular failing case, you can add this seed to the test.

If you're using Clojure, you can add :undertaker.core/seed to this test's options map:
(defprop %s {:seed %s} ...)

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
          (::seed results)
          name))

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

(defn fill-numeric-array [output-arr get-bytes-fn floor ceiling]
  (let [maxes (get-bytes-fn ceiling)
        mins (get-bytes-fn floor)
        first-genned (->> (util/signed-range->generator-range (first mins) (first maxes))
                          (source/get-ubyte *source*)
                          (util/map-generated-byte-into-signed-range (first mins) (first maxes)))
        negative? (neg? first-genned)
        maxes (if (and negative? (not (neg? ceiling)))
                (get-bytes-fn (min -1 ceiling))             ;If we've already generated a -ve number, then the max is actually -1
                maxes)
        mins (if (and (not negative?) (neg? floor))
               (get-bytes-fn (max 0 floor))                 ;Conversely, if we've generated a +ve number, then the minimum is now zero.
               mins)]
    (aset-byte output-arr 0 first-genned)
    (loop [idx 1
           all-maxes? (is-max? first-genned 0 mins maxes)]
      (let [next-val (generate-next-byte-for-int *source* idx all-maxes? mins maxes)]
        (aset-byte output-arr idx next-val)
        (when (< (inc idx) (count output-arr))
          (recur (inc idx)
                 (and all-maxes? (is-max? next-val idx mins maxes))))))
    output-arr))

(defn unsigned-range [floor ceiling]
  (let [unsigned-floor (bit-and 0xff floor)
        unsigned-ceiling (bit-and 0xff ceiling)]
    (unchecked-byte (- (max unsigned-floor unsigned-ceiling) (min unsigned-floor unsigned-ceiling)))))

(defn map-into-unsigned-range [value floor ceiling]
  (let [[floor ceiling] [(min floor ceiling) (max floor ceiling)]]
    (cond
      (and (or (pos? value) (zero? value)) (pos? ceiling) (>= ceiling value)) value
      (and (pos? ceiling) (pos? value)) (+ -129 (- value ceiling))
      (and (zero? ceiling) (neg? floor) (zero? value)) 0
      (and (or (zero? ceiling) (neg? ceiling)) (neg? floor) (not (neg? value))) (+ -128 value)
      (neg? value) (+ -129 (util/abs value)))))

;TODO: refactor
(defn fill-unsigned-numeric-array
  ([output-arr get-bytes-fn]
   (loop [idx 0]
     (let [next-val (generate-next-byte-for-double *source* idx false [] [] [])]
       (aset-byte output-arr idx next-val)
       (when (< (inc idx) (count output-arr))
         (recur (inc idx)))))
   output-arr)
  ([output-arr get-bytes-fn disallowed-values]
   (loop [idx 0]
     (let [disallowed-values (potentially-matched-disallowed-values (take idx output-arr) disallowed-values)
           next-val (generate-next-byte-for-double *source* idx false [] [] disallowed-values)]
       (aset-byte output-arr idx next-val)
       (when (< (inc idx) (count output-arr))
         (recur (inc idx)))))
   output-arr)
  ([output-arr get-bytes-fn floor ceiling] (fill-unsigned-numeric-array output-arr get-bytes-fn floor ceiling #{}))
  ([output-arr get-bytes-fn floor ceiling disallowed-values]
   (let [maxes (get-bytes-fn ceiling)
         mins (get-bytes-fn floor)
         first-genned (-> (source/get-ubyte *source* (unsigned-range (first mins) (first maxes)))
                          (map-into-unsigned-range (first mins) (first maxes))) ;Not sure about this bit, yet.
         negative? (neg? first-genned)
         maxes (if (and negative? (not (neg? ceiling)))     ;not sure if I need these.
                 (get-bytes-fn (min -1 ceiling))            ;If we've already generated a -ve number, then the max is actually -1
                 maxes)
         mins (if (and (not negative?) (neg? floor))
                (get-bytes-fn (max 0 floor))                ;Conversely, if we've generated a +ve number, then the minimum is now zero.
                mins)
         ranges (map unsigned-range mins maxes)]
     (aset-byte output-arr 0 first-genned)
     (loop [idx 1
            all-maxes? (is-max? first-genned 0 mins maxes)]
       (let [disallowed-values (potentially-matched-disallowed-values output-arr disallowed-values)
             next-val (generate-next-byte-for-double *source* idx all-maxes? mins maxes disallowed-values)]
         (aset-byte output-arr idx next-val)
         (when (< (inc idx) (count output-arr))
           (recur (inc idx)
                  (and all-maxes? (is-max? next-val idx mins maxes))))))
     output-arr)))

;TODO: should really be any? not double?
(s/def ::get-bytes-fn (s/fspec :args (s/cat :val double?) :ret bytes?))

(s/fdef fill-unsigned-numeric-array
  :args (s/alt
          :without-ranges (s/cat :output-arr bytes?
                                 :get-bytes-fn (s/with-gen ::get-bytes-fn #(gen/return util/get-bytes-from-double))
                                 :disallowed (s/? (s/coll-of ::util/bytes)))
          :with-ranges (s/cat :output-arr bytes?
                              :get-bytes-fn (s/with-gen ::get-bytes-fn #(gen/return util/get-bytes-from-double))
                              :floor number?
                              :ceiling number?
                              :disallowed (s/? (s/coll-of ::util/bytes))))
  :ret bytes?)

;; === === === === === === === ===
;; Public api
;; === === === === === === === ===

(defn bool
  ([]
   (with-interval (format-interval-name "bool")
     (if (= 1 (source/get-ubyte *source* 1))
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
     (->> (util/signed-range->generator-range min max)
          (source/get-ubyte *source*)
          (util/map-generated-byte-into-signed-range min max)))))

(s/fdef byte
  :args (s/cat :min (s/? ::util/byte) :max (s/? ::util/byte))
  :ret ::util/byte
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Byte/MIN_VALUE
                      max Byte/MAX_VALUE}} args]
          (and (<= min ret)
               (<= ret max)))))

(defn int
  ([] (int Integer/MIN_VALUE Integer/MAX_VALUE))
  ([min] (int min Integer/MAX_VALUE))
  ([floor ceiling]
   (with-interval (format-interval-name "int" floor ceiling)
     (-> (byte-array 4)
         (fill-numeric-array util/get-bytes-from-int floor ceiling)
         (bytes->int)))))

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
     (-> (byte-array 8)
         (fill-numeric-array util/get-bytes-from-long floor ceiling)
         (bytes->long)))))

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

;This is another tricky case.
;Have to deal with the exponent and mantissa separately I think.
;Seems like the bytes in a double (at least in the exponent) are treated as unsigned.
;I.e. -1 > -2

(defn double
  ([]
   (with-interval (format-interval-name "double")
     (-> (byte-array 8)
         (fill-unsigned-numeric-array util/get-bytes-from-double)
         (bytes->double))))
  ([min] (double min Double/MAX_VALUE))
  ([floor ceiling]
   (with-interval (format-interval-name "double" floor ceiling)
     (-> (byte-array 8)
         (fill-unsigned-numeric-array util/get-bytes-from-double floor ceiling)
         (bytes->double)))))

(s/fdef double
  :args (s/cat :floor (s/? double?) :ceiling (s/? double?))
  :ret double?
  :fn (fn [{:keys [args ret]}] (let [{:keys [floor ceiling]
                                      :or   {floor   (- Double/MAX_VALUE)
                                             ceiling Double/MAX_VALUE}} args]
                                 (or (= Double/NaN ret)
                                     (not (Double/isFinite ret))
                                     (and (>= ret floor)
                                          (<= ret ceiling))))))

(def start-of-NaN-values (->> (range -1 -17 -1)
                              (map (fn [i] [127 i]))
                              (set)))

(defn double-without-NaN
  ([]
   (with-interval (format-interval-name "double-without-NaN")
     (-> (byte-array 8)
         (fill-unsigned-numeric-array util/get-bytes-from-double start-of-NaN-values)
         (bytes->double))))
  ([min] (double-without-NaN min Double/MAX_VALUE))
  ([floor ceiling]
   (with-interval (format-interval-name "double-without-NaN" floor ceiling)
     (-> (byte-array 8)
         (fill-unsigned-numeric-array util/get-bytes-from-double floor ceiling start-of-NaN-values)
         (bytes->double)))))

(s/fdef double-without-NaN
  :args (s/cat :floor (s/? double?) :ceiling (s/? double?))
  :ret double?
  :fn (fn [{:keys [args ret]}] (let [{:keys [floor ceiling]
                                      :or   {floor   Double/MIN_VALUE
                                             ceiling Double/MAX_VALUE}} args]
                                 (or (Double/isFinite ret)
                                     (and (>= ret floor)
                                          (<= ret ceiling))))))

(def default-max-size 64)

;TODO bias this so it's more likely to produce longer seqs.
(defn should-generate-elem? [source min max len]
  (with-interval (format-interval-name "should-generate-elem" min max len)
    (= 1 (let [value (source/get-ubyte source 1)]           ;Side-effecty
           (cond (> min len) 1
                 (< max (inc len)) 0
                 :default value)))))

(defn vec-of
  ([elem-gen] (vec-of elem-gen 0))
  ([elem-gen min] (vec-of elem-gen min (+ min default-max-size)))
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
           (println message#))
         (when (:debug ~opts)
           (println "\n\nDebug output follows:\n")
           (println result#))))))
