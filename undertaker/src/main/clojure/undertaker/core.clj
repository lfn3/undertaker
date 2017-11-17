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
  (fn [msg]
    (swap! an-atom conj msg)))

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
        result-map (-> (f source)
                       (assoc ::intervals (source/get-intervals source))
                       (assoc ::generated-bytes (vec (source/get-sourced-bytes source)))
                       (assoc ::generated-values (map ::proto/generated-value
                                                      (->> source
                                                           (source/get-intervals)
                                                           (filter (comp zero? ::proto/interval-depth))))))]
    (if (::result result-map)
      result-map
      (let [shrunk-source (shrink/shrink (source/get-sourced-bytes source) (source/get-intervals source) f)
            shrunk-intervals (source/get-intervals shrunk-source)]
        (-> result-map
            (assoc ::shrunk-intervals shrunk-intervals)
            (assoc ::shrunk-bytes (vec (source/get-sourced-bytes shrunk-source)))
            (assoc ::shrunk-values (->> shrunk-intervals
                                        (filter (comp zero? ::proto/interval-depth))
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
          (bytes/bytes->byte)))))

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
  ([floor ceiling & more-ranges]
   (with-interval 
     (->> (bytes/split-number-line-ranges-into-bytewise-min-max (concat [floor ceiling] more-ranges) bytes/int->bytes)
          (source/get-bytes *source*)
          (bytes/bytes->int)))))

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
          (first)
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
          (first)
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
          (first)
          (core/char)))))

(defn long
  ([] (long Long/MIN_VALUE Long/MAX_VALUE))
  ([min] (long min Long/MAX_VALUE))
  ([floor ceiling]
   (with-interval 
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
   (with-interval 
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
   (with-interval 
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
   (with-interval 
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

(defn repeat-range [ranges size]
  (->> ranges
       (map (comp vec (partial map (comp vec (partial mapcat (partial repeat size))))))
       (vec)))

(s/fdef repeat-range
  :args (s/cat :ranges ::bytes/ranges :size int?)
  :ret ::bytes/ranges
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [size]} args]
          (= size (count (first (first ret)))))))

;TODO bias this so it's more likely to produce longer seqs.
(defn should-generate-elem? [floor ceiling len]
  (with-interval 
    (<= 1 (let [value (byte 0 5)]                            ;Side-effecty
           (cond (< len floor) 1
                 (< ceiling (inc len)) 0
                 :default value)))))

(def default-collection-max-size 64)

(defn vec-of
  ([elem-gen] (vec-of elem-gen 0))
  ([elem-gen min] (vec-of elem-gen min (+ min default-collection-max-size)))
  ([elem-gen min max]
   (with-interval 
     (loop [result []]
       (let [i (count result)]
         (if-let [next (with-interval
                         (when-let [gen-next? (should-generate-elem? min max i)]
                          (elem-gen)))]
           (recur (conj result next))
           result))))))

(defn set-of
  ([elem-gen min max]
   (let [uniqueness-id (swap! set-uniqueness-id inc)]
     (loop [result #{}]
       (let [i (count result)]
         (if-let [next (with-interval
                         (when-let [gen-next? (should-generate-elem? min max i)]
                           (with-interval-and-hints [[::proto/immediate-children-of ::proto/unique uniqueness-id]]
                                                    (elem-gen))))]
           (recur (conj result next))
           result))))))

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
  ([kv-gen]
   (with-interval 
     (loop [result []]
       (let [i (count result)]
         (if-let [next (with-interval 
                         (when-let [gen-next? (should-generate-elem? 0 64 i)]
                           (kv-gen)))]
           (recur (conj result next))
           (into {} result)))))))

(s/fdef map-of
  :args (s/cat :kv-gen (s/fspec :args (s/cat)
                                :ret (s/tuple any? any?)))
  :ret map?)

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
         (when debug/debug-mode
           (println "\n\nDebug output follows:\n")
           (println result#))))))
