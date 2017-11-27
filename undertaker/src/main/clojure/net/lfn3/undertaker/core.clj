(ns net.lfn3.undertaker.core
  (:refer-clojure :exclude [int byte long double short char float keyword])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [clojure.test :as t]
            [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.source :as source]
            [net.lfn3.undertaker.source.multi :as source.multi]
            [net.lfn3.undertaker.source.fixed :as source.fixed]
            [net.lfn3.undertaker.source.sample :as source.sample]
            [clojure.test.check.generators :as gen]
            [net.lfn3.undertaker.shrink :as shrink]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.messages :as messages]
            [net.lfn3.undertaker.debug :as debug])
  (:import (java.util Random Arrays)
           (java.nio ByteBuffer)
           (net.lfn3.undertaker OverrunException UndertakerDebugException)
           (net.lfn3.undertaker.source.sample SampleSource)))

(defonce seed-uniquifier* (volatile! (core/long 8682522807148012)))

(defn seed-uniquifier []
  (vswap! seed-uniquifier* #(unchecked-multiply (core/long %1) ;TODO: get rid of casts.
                                                (core/long 181783497276652981))))

(defn next-seed [seed]
  (bit-xor (seed-uniquifier) (inc seed)))

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

(defn format-results [name results]
  (cond
    (and (not (::source-used results)) (::result results)) (messages/format-not-property-passed name results)
    (not (::source-used results)) (messages/format-not-property-test-failed name results)
    (not (::result results)) (messages/format-failed name results)
    :default nil))

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

(defn bool
  ([]
   (with-interval
     (if (= 1 (byte 0 1))
       true
       false))))

(defn short
  ([] (short Short/MIN_VALUE Short/MAX_VALUE))
  ([min] (short min Short/MAX_VALUE))
  ([floor ceiling]
   (with-interval
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/short->bytes)
          (source/get-bytes *source*)
          (.getShort)))))

(defn int
  ([] (int Integer/MIN_VALUE Integer/MAX_VALUE))
  ([min] (int min Integer/MAX_VALUE))
  ([floor ceiling & more-ranges]
   (with-interval
     (->> (bytes/split-number-line-ranges-into-bytewise-min-max (concat [floor ceiling] more-ranges) bytes/int->bytes)
          (source/get-bytes *source*)
          (.getInt)))))

(defn char
  "Returns a java primitive char. Does not generate values outside the BMP (Basic Multilingual Plane)."
  ([] (core/char (int 0x0000 0xD800))))

(def ascii-range [[[32] [126]]])

(defn char-ascii
  "Generates printable ascii characters"
  ([]
   (with-interval
     (->> ascii-range
          (source/get-bytes *source*)
          (.get)
          (core/char)))))

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

(defn float
  ([] (float (- Double/MAX_VALUE) Double/MAX_VALUE))
  ([min] (float min Double/MAX_VALUE))
  ([floor ceiling]
   (with-interval
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/float->bytes)
          (source/get-bytes *source*)
          (.getFloat)))))

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

(defn double
  ([] (real-double (- Double/MAX_VALUE) Double/MAX_VALUE))
  ([min] (real-double min Double/MAX_VALUE))
  ([floor ceiling]
   (with-interval
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/double->bytes)
          (source/get-bytes *source*)
          (.getDouble)))))

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
   `(with-interval
       (loop [result# (~collection-init-fn)
              i# 0]
         (if-let [next# (with-interval-and-hints [[::proto/this ::proto/snippable nil]]
                          (when-let [gen-next?# (should-generate-elem? ~min-size ~max-size i#)]
                            (~generation-fn)))]
           (recur (~add-to-coll-fn result# next#) (inc i#))
           result#)))))

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

(defn string-ascii
  ([] (string-ascii 0 default-string-max-size))
  ([min] (string-ascii min (+ default-string-max-size min)))
  ([min max]
   (with-interval
     (-> (vec-of char-ascii min max)
         (char-array)
         (String.)))))

(defn string-alphanumeric
  ([] (string-alphanumeric 0 default-string-max-size))
  ([min] (string-alphanumeric min (+ default-string-max-size min)))
  ([min max]
   (with-interval
     (-> (vec-of char-alphanumeric min max)
         (char-array)
         (String.)))))

(defn string-alpha
  ([] (string-alpha 0 default-string-max-size))
  ([min] (string-alpha min (+ default-string-max-size min)))
  ([min max]
   (with-interval
     (-> (vec-of char-alpha min max)
         (char-array)
         (String.)))))

(defn from
  "Pick a random value from the supplied collection. Shrinks to the first element of the collection."
  ([coll]
   (with-interval
     (let [target-idx (int 0 (dec (count coll)))]
       (nth (vec coll) target-idx)))))

(def char-symbol-special
  "Non-alphanumeric characters that can be in a symbol."
  [\* \+ \! \- \_ \?])

(defn keyword []
  (with-interval "keyword"
    (->> (concat [(from char-symbol-special)] (vec-of char-alphanumeric))
         (apply str)
         (core/keyword))))

(defn map-of
  ([key-gen value-gen] (map-of key-gen value-gen 0 default-collection-max-size))
  ([key-gen value-gen min-size] (map-of key-gen value-gen min-size (+ min-size default-collection-max-size)))
  ([key-gen value-gen min-size max-size] (map-of key-gen value-gen min-size max-size {}))
  ([key-gen value-gen min-size max-size {:keys [value-gen-takes-key-as-arg]}]
   (with-interval
     (let [uniqueness-id (swap! set-uniqueness-id inc)
           kv-gen #(let [k (with-interval-and-hints [[::proto/immediate-children-of ::proto/unique uniqueness-id]]
                             (key-gen))
                         v (if value-gen-takes-key-as-arg
                             (value-gen k)
                             (value-gen))]
                     [k v])]
       (into {} (collection vector kv-gen conj min-size max-size))))))

(def any-gens #{bool
                int})

(defn any
  ([] (any #{}))
  ([exclusions]
   (with-interval
     (let [chosen-generator (from (remove exclusions any-gens))]
       (chosen-generator)))))

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
