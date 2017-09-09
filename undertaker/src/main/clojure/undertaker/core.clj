(ns undertaker.core
  (:gen-class)
  (:refer-clojure :exclude [int byte long double short char float])
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
            [undertaker.shrink :as shrink]
            [undertaker.bytes :as bytes])
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

(defn format-interval-name [name & args]
  (str name " [" (str/join " " args) "]"))

(defmacro with-interval [name & body]
  `(let [interval-token# (source/push-interval *source* ~name)]
     (try
       (let [result# (do ~@body)]
         (source/pop-interval *source* interval-token# result#)
         result#)
       (catch Exception e#
         (source/pop-interval *source* interval-token# nil)
         (throw e#)))))

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
(s/def ::disallowed-values (s/coll-of ::util/bytes))

(s/fdef run-prop
  :args (s/cat :opts-map ::prop-opts-map
               :fn fn?)
  :ret ::results-map)

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
  :args (s/cat :name string? :results ::results-map)
  :ret (s/nilable string?))

;; === === === === === === === ===
;; Public api
;; === === === === === === === ===

(defn byte
  ([] (byte Byte/MIN_VALUE Byte/MAX_VALUE))
  ([min] (byte min Byte/MAX_VALUE))
  ([min max]
   (with-interval (format-interval-name "byte" min max)     ;This is slightly ridiculous, but consistency is key!
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max min max bytes/byte->bytes)
          (source/get-bytes *source*)
          (bytes/bytes->byte)))))

(s/fdef byte
  :args (s/cat :min (s/? ::util/byte) :max (s/? ::util/byte))
  :ret ::util/byte
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Byte/MIN_VALUE
                      max Byte/MAX_VALUE}} args]
          (and (<= min ret)
               (<= ret max)))))

(defn bool
  ([]
   (with-interval (format-interval-name "bool")
     (if (= 1 (byte 0 1))
       true
       false))))

(s/fdef bool
  :args (s/cat)
  :ret boolean?)

(defn short
  ([] (short Short/MIN_VALUE Short/MAX_VALUE))
  ([min] (short min Short/MAX_VALUE))
  ([floor ceiling]
   (with-interval (format-interval-name "short" floor ceiling)
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/short->bytes)
          (source/get-bytes *source*)
          (bytes/bytes->short)))))

(s/fdef short
  :args (s/cat :min (s/? int?)
               :max (s/? int?))
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Short/MIN_VALUE
                      max Short/MAX_VALUE}} args]
          (and (<= min ret)
               (>= max ret)))))

(defn int
  ([] (int Integer/MIN_VALUE Integer/MAX_VALUE))
  ([min] (int min Integer/MAX_VALUE))
  ([floor ceiling]
   (with-interval (format-interval-name "int" floor ceiling)
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/int->bytes)
          (source/get-bytes *source*)
          (bytes/bytes->int)))))

(s/fdef int
  :args (s/cat :min (s/? int?)
               :max (s/? int?))
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Integer/MIN_VALUE
                      max Integer/MAX_VALUE}} args]
          (and (<= min ret)
               (>= max ret)))))

(defn char
  "Returns a java primitive char. Does not generate values outside the BMP (Basic Multilingual Plane)."
  ([] (clojure.core/char (int 0x0000 0xD800))))

(s/fdef char
  :args (s/cat)
  :ret char?)

(defn long
  ([] (long Long/MIN_VALUE Long/MAX_VALUE))
  ([min] (long min Long/MAX_VALUE))
  ([floor ceiling]
   (with-interval (format-interval-name "long" floor ceiling)
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/long->bytes)
          (source/get-bytes *source*)
          (bytes/bytes->long)))))

(s/fdef long
  :args (s/cat :min (s/? int?)
               :max (s/? int?))
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Long/MIN_VALUE
                      max Long/MAX_VALUE}} args]
          (and (<= min ret)
               (>= max ret)))))

(defn float
  ([] (float (- Double/MAX_VALUE) Double/MAX_VALUE))
  ([min] (float min Double/MAX_VALUE))
  ([floor ceiling]
   (with-interval (format-interval-name "float" floor ceiling)
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/float->bytes)
          (source/get-bytes *source*)
          (bytes/bytes->float)))))

(s/fdef float
  :args (s/cat :floor (s/? float?) :ceiling (s/? float?))
  :ret float?
  :fn (fn [{:keys [args ret]}] (let [{:keys [floor ceiling]
                                      :or   {floor   (- Float/MAX_VALUE)
                                             ceiling Float/MAX_VALUE}} args]
                                 (or (= Float/NaN ret)
                                     (not (Float/isFinite ret))
                                     (and (>= ret floor)
                                          (<= ret ceiling))))))

(def start-of-unreal-doubles (->> (range -1 -17 -1)
                                  (mapcat (fn [i] [[127 i] [-1 i]]))
                                  (set)))

(defn real-double
  ([] (real-double (- Double/MAX_VALUE) Double/MAX_VALUE))
  ([min] (real-double min Double/MAX_VALUE))
  ([floor ceiling]
   (with-interval (format-interval-name "real-double" floor ceiling)
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/double->bytes)
          (source/get-bytes *source* start-of-unreal-doubles)
          (bytes/bytes->double)))))

(s/fdef real-double
  :args (s/cat :floor (s/? double?) :ceiling (s/? double?))
  :ret double?
  :fn (fn [{:keys [args ret]}] (let [{:keys [floor ceiling]
                                      :or   {floor   (- Double/MAX_VALUE)
                                             ceiling Double/MAX_VALUE}} args]
                                 (and (>= ret floor)
                                      (<= ret ceiling)))))

(defn double
  ([] (real-double (- Double/MAX_VALUE) Double/MAX_VALUE))
  ([min] (real-double min Double/MAX_VALUE))
  ([floor ceiling]
   (with-interval (format-interval-name "double" floor ceiling)
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/double->bytes)
          (source/get-bytes *source*)
          (bytes/bytes->double)))))

(s/fdef double
  :args (s/cat :floor (s/? double?) :ceiling (s/? double?))
  :ret double?
  :fn (fn [{:keys [args ret]}] (let [{:keys [floor ceiling]
                                      :or   {floor   (- Double/MAX_VALUE)
                                             ceiling Double/MAX_VALUE}} args]
                                 (or (Double/isNaN ret)
                                     (Double/isInfinite ret)
                                     (and (<= floor ret)
                                          (<= ret ceiling))))))

(def default-string-max-size 2048)

(defn string
  ([] (string 0 default-string-max-size))
  ([min] (string min (+ default-string-max-size min)))
  ([min max]
   (with-interval (format-interval-name "string" min max)
     (let [size (int min max)]
       (-> (source/get-bytes *source* [[(vec (repeat size -128)) (vec (repeat size -1))]
                                       [(vec (repeat size 0)) (vec (repeat size 127))]])
           (String.))))))

(def default-collection-max-size 64)

;TODO bias this so it's more likely to produce longer seqs.
(defn should-generate-elem? [min max len]
  (with-interval (format-interval-name "should-generate-elem" min max len)
    (= 1 (let [value (byte 0 1)]                            ;Side-effecty
           (cond (> min len) 1
                 (< max (inc len)) 0
                 :default value)))))

(defn vec-of
  ([elem-gen] (vec-of elem-gen 0))
  ([elem-gen min] (vec-of elem-gen min (+ min default-collection-max-size)))
  ([elem-gen min max]
   (with-interval (format-interval-name "vec" min max)
     (loop [result []]
       (let [i (count result)]
         (if-let [next (with-interval (format-interval-name "chunk for vector" i)
                         (when-let [gen-next? (should-generate-elem? min max i)]
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
