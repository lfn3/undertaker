(ns undertaker.core
  (:refer-clojure :exclude [int byte long double short char float keyword])
  (:require [clojure.core :as core]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [clojure.string :as str]
            [clojure.test :as t]
            [undertaker.proto :as proto]
            [undertaker.source :as source]
            [undertaker.source.multi :as source.multi]
            [undertaker.source.fixed :as source.fixed]
            [undertaker.source.sample :as source.sample]
            [clojure.test.check.generators :as gen]
            [undertaker.shrink :as shrink]
            [undertaker.bytes :as bytes]
            [undertaker.messages :as messages]
            [undertaker.debug :as debug])
  (:import (java.util Random Arrays)
           (java.nio ByteBuffer)
           (com.lmax.undertaker OverrunException UndertakerDebugException)
           (undertaker.source.sample SampleSource)))

(defonce seed-uniquifier* (volatile! (core/long 8682522807148012)))

(defn seed-uniquifier []
  (vswap! seed-uniquifier* #(unchecked-multiply (core/long %1) ;TODO: get rid of casts.
                                                (core/long 181783497276652981))))

(defn next-seed [seed]
  (bit-xor (seed-uniquifier) (inc seed)))

(s/fdef next-seed
  :args (s/cat :seed integer?)
  :ret integer?)

(def ^:dynamic *source* (source.sample/make-source (System/nanoTime)))

(def set-uniqueness-id (atom 0))

(defmacro with-interval-and-hints [hints & body]
  `(do
     (source/push-interval *source* ~hints)
     (let [result# (do ~@body)]
       (source/pop-interval *source* result#)
       result#)))

(defmacro with-interval [& body]
  `(with-interval-and-hints #{} ~@body))

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
  (fn [msg] (swap! an-atom conj msg)))

(defn wrap-fn [f]
  (fn [source]
    (let [result (atom [])
          report-fn (make-report-fn result)]
      (when-not (instance? SampleSource *source*)
        (throw (IllegalStateException. (messages/already-bound-source-error-string))))
      (with-bindings {#'t/report report-fn
                      #'*source* source}
        (try
          (f)
          {::result   (check-test-reports @result)
           ::cause    (get-failures-from-test-reports @result)
           ::reported @result}
          (catch Throwable e
            (when (or (instance? UndertakerDebugException e)
                      (and (instance? IllegalStateException e)
                           (instance? UndertakerDebugException (.getCause e))))
              (throw e))
            {::result   false
             ::cause    e
             ::reported @result})
          (finally
            (reset! result [])))))))

(defn run-prop-1 [source f]
  (let [f (wrap-fn f)
        result-map (f source)
        success? (::result result-map)
        result-map (cond-> result-map
                     debug/debug-mode (assoc ::intervals (source/get-intervals source))
                     debug/debug-mode (assoc ::generated-bytes (-> source
                                                                   (source/get-sourced-bytes)
                                                                   (.array)
                                                                   (vec)))
                     (not success?) (assoc ::generated-values
                                           (map ::proto/generated-value
                                                (->> source
                                                     (source/get-intervals)
                                                     (filter (comp zero? ::proto/interval-depth))))))]
    (if success?
      result-map
      (let [shrunk-source (shrink/shrink source f)
            shrunk-intervals (try (source/get-intervals shrunk-source)
                                  (catch UndertakerDebugException e))]
        (cond-> result-map
            debug/debug-mode (assoc ::shrunk-intervals shrunk-intervals)
            debug/debug-mode (assoc ::shrunk-bytes (-> shrunk-source
                                                       (source/get-sourced-bytes)
                                                       (.array)
                                                       (vec)))
            true (assoc ::shrunk-values (->> shrunk-intervals
                                             (filter (comp zero? ::proto/interval-depth))
                                             (map ::proto/generated-value))))))))

(s/def ::result boolean?)
(s/def ::generated-values any?)
(s/def ::shrunk-values any?)
(s/def ::seed integer?)
(s/def ::results-map (s/keys :req [::result]
                             :opt [::shrunk-values ::seed ::generated-values]))

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
                          (assoc ::source source)
                          (assoc ::source-used (source/used? source))
                          (assoc ::iterations-run (- iterations (dec iterations-left)))
                          (assoc ::seed seed)))))]
     (source/done-with-test!)
     result)))

(s/def ::iterations integer?)
(s/def ::prop-opts-map (s/keys :opt-un [::seed ::iterations]))
(s/def ::disallowed-values (s/coll-of ::bytes/bytes))

(s/fdef run-prop
  :args (s/cat :opts-map ::prop-opts-map
               :fn fn?)
  :ret ::results-map)

(defn format-results [name results]
  (cond
    (and (not (::source-used results)) (::result results)) (messages/format-not-property-passed name results)
    (not (::source-used results)) (messages/format-not-property-test-failed name results)
    (not (::result results)) (messages/format-failed name results)
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
   (with-interval      ;This is slightly ridiculous, but consistency is key!
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max min max bytes/byte->bytes)
          (source/get-bytes *source*)
          (.get)))))

(s/fdef byte
  :args (s/cat :min (s/? ::bytes/byte) :max (s/? ::bytes/byte))
  :ret ::bytes/byte
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Byte/MIN_VALUE
                      max Byte/MAX_VALUE}} args]
          (and (<= min ret)
               (<= ret max)))))

(defn bool
  ([]
   (with-interval
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
   (with-interval
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/short->bytes)
          (source/get-bytes *source*)
          (.getShort)))))

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
  ([floor ceiling & more-ranges]
   (with-interval
     (->> (bytes/split-number-line-ranges-into-bytewise-min-max (concat [floor ceiling] more-ranges) bytes/int->bytes)
          (source/get-bytes *source*)
          (.getInt)))))

(s/fdef int
  :args (s/cat :floor (s/? int?)
               :ceiling (s/? int?)
               :more-ranges (s/* int?))
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [floor ceiling more-ranges]
               :or   {floor       Integer/MIN_VALUE
                      ceiling     Integer/MAX_VALUE
                      more-ranges []}} args
              total-floor (apply min floor more-ranges)
              total-ceiling (apply max ceiling more-ranges)]
          (and (<= total-floor ret)
               (>= total-ceiling ret)
               (even? (count more-ranges))))))

(defn char
  "Returns a java primitive char. Does not generate values outside the BMP (Basic Multilingual Plane)."
  ([] (core/char (int 0x0000 0xD800))))

(s/fdef char
  :args (s/cat)
  :ret char?)

(def ascii-range [[[32] [126]]])

(defn char-ascii
  "Generates printable ascii characters"
  ([]
   (with-interval
     (->> ascii-range
          (source/get-bytes *source*)
          (.get)
          (core/char)))))

(s/fdef char-ascii
  :args (s/cat)
  :ret char?)                                               ;TODO: check against set in undertaker.usage. Need to move it to a different ns first though.

(def alphanumeric-range [[[48] [57]]
                         [[65] [90]]
                         [[97] [122]]])

(defn char-alphanumeric
  "Generates characters 0 -> 9, a -> z and A -> Z"
  ([]
   (with-interval
     (->> alphanumeric-range
          (source/get-bytes *source*)
          (.get)
          (core/char)))))

(s/fdef char-ascii
  :args (s/cat)
  :ret char?)                                               ;TODO: check against set in undertaker.usage. Need to move it to a different ns first though.

(def alpha-range [[[65] [90]]
                  [[97] [122]]])

(defn char-alpha
  "Generates characters a -> z and A -> Z"
  ([]
   (with-interval
     (->> alpha-range
          (source/get-bytes *source*)
          (.get)
          (core/char)))))

(defn long
  ([] (long Long/MIN_VALUE Long/MAX_VALUE))
  ([min] (long min Long/MAX_VALUE))
  ([floor ceiling]
   (with-interval
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/long->bytes)
          (source/get-bytes *source*)
          (.getLong)))))

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
   (with-interval
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/float->bytes)
          (source/get-bytes *source*)
          (.getFloat)))))

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
   (with-interval
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/double->bytes)
          (source/get-bytes *source* start-of-unreal-doubles)
          (.getDouble)))))

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
   (with-interval
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/double->bytes)
          (source/get-bytes *source*)
          (.getDouble)))))

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

;TODO bias this so it's more likely to produce longer seqs.
(defn should-generate-elem? [floor ceiling len]
  (with-interval
    (<= 1 (let [value (byte 0 5)]                            ;Side-effecty
           (cond (< len floor) 1
                 (< ceiling (inc len)) 0
                 :default value)))))

(def default-collection-max-size 64)

(defmacro collection
  ([collection-init-fn generation-fn add-to-coll-fn min-size max-size]
   `(collection ~collection-init-fn ~generation-fn ~add-to-coll-fn identity ~min-size ~max-size))
  ([collection-init-fn generation-fn add-to-coll-fn conversion-fn min-size max-size]
   `(with-interval
      (loop [result# (~collection-init-fn)
             i# 0]
        (if-let [next# (with-interval-and-hints [[::proto/this ::proto/snippable nil]]
                                                (when-let [gen-next?# (should-generate-elem? ~min-size ~max-size i#)]
                                                  (~generation-fn)))]
          (recur (~add-to-coll-fn result# next#) (inc i#))
          (~conversion-fn result#))))))

(defn vec-of
  ([elem-gen] (vec-of elem-gen 0))
  ([elem-gen min] (vec-of elem-gen min (+ min default-collection-max-size)))
  ([elem-gen min max] (collection vector elem-gen conj min max)))

(defn set-of
  ([elem-gen min max]
   (let [uniqueness-id (swap! set-uniqueness-id inc)]
     (collection (fn [] #{})
                 #(with-interval-and-hints [[::proto/immediate-children-of ::proto/unique uniqueness-id]]
                                           (elem-gen))
                 conj
                 min
                 max))))

(defn string
  ([] (string 0 default-string-max-size))
  ([min] (string min (+ default-string-max-size min)))
  ([min max]
   (with-interval
     (-> (vec-of char min max)
         (char-array)
         (String.)))))

(s/fdef string
  :args (s/cat :min (s/? int?) :max (s/? int?))
  :ret string?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min 0
                      max (+ min default-string-max-size)}} args]
          (and (<= min (count ret))
               (<= (count ret) max)))))

(defn string-ascii
  ([] (string-ascii 0 default-string-max-size))
  ([min] (string-ascii min (+ default-string-max-size min)))
  ([min max]
   (with-interval
     (-> (vec-of char-ascii min max)
         (char-array)
         (String.)))))

(s/fdef string-ascii
  :args (s/cat :min (s/? int?) :max (s/? int?))
  :ret string?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min 0
                      max (+ min default-string-max-size)}} args]
          (and (<= min (count ret))
               (<= (count ret) max)))))

(defn string-alphanumeric
  ([] (string-alphanumeric 0 default-string-max-size))
  ([min] (string-alphanumeric min (+ default-string-max-size min)))
  ([min max]
   (with-interval
     (-> (vec-of char-alphanumeric min max)
         (char-array)
         (String.)))))

(s/fdef string-alphanumeric
  :args (s/cat :min (s/? int?) :max (s/? int?))
  :ret string?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min 0
                      max (+ min default-string-max-size)}} args]
          (and (<= min (count ret))
               (<= (count ret) max)))))

(defn string-alpha
  ([] (string-alpha 0 default-string-max-size))
  ([min] (string-alpha min (+ default-string-max-size min)))
  ([min max]
   (with-interval
     (-> (vec-of char-alpha min max)
         (char-array)
         (String.)))))

(s/fdef string-alpha
  :args (s/cat :min (s/? int?) :max (s/? int?))
  :ret string?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min 0
                      max (+ min default-string-max-size)}} args]
          (and (<= min (count ret))
               (<= (count ret) max)))))

(defn from
  "Pick a random value from the supplied collection. Shrinks to the first element of the collection."
  ([coll]
   (with-interval
     (let [target-idx (int 0 (dec (count coll)))]
       (nth (vec coll) target-idx)))))

(s/fdef from
  :args (s/cat :coll (s/coll-of any?))
  :ret any?
  :fn (fn [{:keys [args ret]}] (contains? (set (:coll args)) ret)))

(def char-symbol-special
  "Non-alphanumeric characters that can be in a symbol."
  [\* \+ \! \- \_ \?])

(defn keyword []
  (with-interval "keyword"
    (->> (concat [(from char-symbol-special)] (vec-of char-alphanumeric))
         (apply str)
         (core/keyword))))

(s/fdef keyword
  :args (s/cat)
  :ret keyword?)

(defn map-of
  ([kv-gen] (map-of kv-gen 0 default-collection-max-size))
  ([kv-gen min-size] (map-of kv-gen min-size (+ min-size default-collection-max-size)))
  ([kv-gen min-size max-size] (collection vector kv-gen conj #(into {} %1) min-size max-size)))

(s/fdef map-of
  :args (s/cat :kv-gen (s/fspec :args (s/cat)
                                :ret (s/tuple any? any?))
               :min-size (s/? nat-int?)
               :max-size (s/? nat-int?))
  :ret map?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min-size max-size]} args]
          (if (and min-size max-size)
            (<= min-size max-size)
            true))))

(def any-gens #{bool
                int})

(defn any
  ([] (any #{}))
  ([exclusions]
   (with-interval
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
