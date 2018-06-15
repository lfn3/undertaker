(ns net.lfn3.undertaker.core
  (:refer-clojure :exclude [int byte long double short char float keyword boolean shuffle symbol list])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [clojure.test :as t]
            [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.source :as source]
            [net.lfn3.undertaker.source.fixed :as source.fixed]
            [net.lfn3.undertaker.shrink :as shrink]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.messages :as messages]
            [net.lfn3.undertaker.source.wrapped-random :as source.random]
            [clojure.pprint])
  (:import (net.lfn3.undertaker UndertakerDebugException UniqueInputValuesExhaustedException)
           (java.util UUID)
           (java.nio ByteBuffer)))

(defonce seed-uniquifier* (volatile! (core/long 8682522807148012)))

(defn seed-uniquifier []
  (vswap! seed-uniquifier* #(unchecked-multiply (core/long %1) ;TODO: get rid of casts.
                                                (core/long 181783497276652981))))

(defn next-seed [seed]
  (bit-xor (seed-uniquifier) (inc seed)))

(def ^:dynamic *source* (net.lfn3.undertaker.source.wrapped-random/make-source (next-seed (System/nanoTime))))

(def unique-hint-ids (atom 0))

(defmacro with-interval-and-hints [type hints & body]
  `(do
    (source/push-interval *source* ~type ~hints)
    (let [result# (do ~@body)]
      (source/pop-interval *source* result#)
      result#)))

(defmacro with-leaf-interval-and-hints [hints & body]
  `(with-interval-and-hints ::proto/leaf-interval ~hints ~@body))

(defmacro with-leaf-interval [& body]
  `(with-leaf-interval-and-hints #{} ~@body))

(defmacro with-compound-interval-and-hints [hints & body]
  `(with-interval-and-hints ::proto/compound-interval ~hints ~@body))

(defmacro with-compound-interval [& body]
  `(with-compound-interval-and-hints #{} ~@body))

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
    (source/starting-test-instance source)
    (let [result (atom [])
          report-fn (make-report-fn result)]
      (with-bindings {#'t/report report-fn
                      #'*source* source}
        (try
          (f)
          {::result   (check-test-reports @result)
           ::cause    (get-failures-from-test-reports @result)
           ::reported @result
           ::proto/source-used? (source/used? source)}
          (catch Throwable e
            (when (or (instance? UndertakerDebugException e)
                      (and (instance? IllegalStateException e)
                           (instance? UndertakerDebugException (.getCause e))))
              (throw e))
            {::result   false
             ::cause    e
             ::reported @result
             ::proto/source-used? (source/used? source)})
          (finally
            (source/completed-test-instance source)
            (reset! result [])))))))

;So we can rebind in tests
(defn print-initial-failure [test-name result-data]
  (println (messages/format-initial-failure test-name result-data)))

(defn do-shrink [f bytes test-name iterations seed]
  (let [initial-failing-source (source.fixed/make-fixed-source bytes)
        run-data-with-intervals (f initial-failing-source)
        _ (print-initial-failure test-name
                                 (-> {::initial-results run-data-with-intervals}
                                     (assoc ::iterations-run iterations)
                                     (assoc ::seed seed)))]
    {::initial-results run-data-with-intervals
     ::shrunk-results (shrink/shrink initial-failing-source f)}))

(defn run-prop
  ([{:keys [::test-name seed iterations debug]
     :or   {seed       (next-seed (System/nanoTime))
            iterations 1000
            debug false}}
    f]
   (let [source (source.random/make-source seed)
         f (wrap-fn f)
         _ (source/starting-test source debug)
         result (loop [iterations-left iterations]
                  (let [run-data (source/add-source-data-to-results-map source (f source))
                        passed? (true? (::result run-data))
                        used? (::proto/source-used? run-data)]
                    (cond (and passed?
                               (> iterations-left 1)
                               used?)                       ;If a source is unused, there isn't much point in rerunning
                          (do                               ;the test, since nothing will change
                            (source/reset source)
                            (recur (dec iterations-left)))
                          (and used?                        ;No point in shrinking if it's unused.
                               (not passed?))
                          (do-shrink f
                                     (source/get-sourced-bytes source)
                                     test-name
                                     (- iterations (dec iterations-left))
                                     seed)
                          :default (-> {::initial-results run-data}
                                       (assoc ::iterations-run (- iterations (dec iterations-left)))
                                       (assoc ::seed seed)))))]
     (source/completed-test source)
     result)))

(defn format-results
  ([name {:keys [::initial-results ::shrunk-results] :as results} failed-lang-fn debug?]
   (cond-> ""
     (and (not (::proto/source-used? initial-results)) (::result initial-results)) (str (messages/format-not-property-passed name results))
     (and (not (::proto/source-used? initial-results)) (not (::result initial-results))) (str (messages/format-not-property-test-failed name results))
     (and (::proto/source-used? initial-results) (not (::result initial-results))) (str (messages/format-shrunk results) (failed-lang-fn name results))
     debug? (str "\n\nDebug output follows:\n" (with-out-str (clojure.pprint/pprint results)))
     true (not-empty))))

(defmacro get-from-byte-buffer-abs [f ^ByteBuffer byte-buffer]
  `(let [buffer# ~byte-buffer]
     (~f buffer# (.position buffer#))))

;; === === === === === === === ===
;; Public api
;; === === === === === === === ===

(defn byte
  ([] (byte Byte/MIN_VALUE Byte/MAX_VALUE))
  ([min] (byte min Byte/MAX_VALUE))
  ([min max]
   (with-leaf-interval      ;This is slightly ridiculous, but consistency is key!
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max min max bytes/byte->bytes)
          (source/get-bytes *source*)
          (get-from-byte-buffer-abs .get)))))

(defn boolean
  ([]
   (with-leaf-interval
     (= 1 (byte 0 1)))))

(defn short
  ([] (short Short/MIN_VALUE Short/MAX_VALUE))
  ([min] (short min Short/MAX_VALUE))
  ([floor ceiling & more-ranges]
   (with-leaf-interval
     (->> (bytes/split-number-line-ranges-into-bytewise-min-max (concat [floor ceiling] more-ranges) bytes/short->bytes)
          (source/get-bytes *source*)
          (get-from-byte-buffer-abs .getShort)))))

(defn nat [] (short 0 200))

(defn int
  ([] (int Integer/MIN_VALUE Integer/MAX_VALUE))
  ([min] (int min Integer/MAX_VALUE))
  ([floor ceiling & more-ranges]
   (with-leaf-interval
     (->> (bytes/split-number-line-ranges-into-bytewise-min-max (concat [floor ceiling] more-ranges) bytes/int->bytes)
          (source/get-bytes *source*)
          (get-from-byte-buffer-abs .getInt)))))

(defn char
  "Returns a java primitive char. Does not generate values outside the BMP (Basic Multilingual Plane)."
  ([] (core/char (int 0x0000 0xD800))))

(defn vector-ranges-to-byte-ranges [ranges]
  (vec (map (comp vec (partial map byte-array)) ranges)))

(def ascii-range (vector-ranges-to-byte-ranges [[[32] [126]]]))

(defn char-ascii
  "Generates printable ascii characters"
  ([]
   (with-leaf-interval
     (->> ascii-range
          (source/get-bytes *source*)
          (get-from-byte-buffer-abs .get)
          (core/char)))))

(def alphanumeric-range (vector-ranges-to-byte-ranges [[[48] [57]]
                                                       [[65] [90]]
                                                       [[97] [122]]]))

(defn char-alphanumeric
  "Generates characters 0 -> 9, a -> z and A -> Z"
  ([]
   (with-leaf-interval
     (->> alphanumeric-range
          (source/get-bytes *source*)
          (get-from-byte-buffer-abs .get)
          (core/char)))))

(def alpha-range (vector-ranges-to-byte-ranges [[[65] [90]]
                                                [[97] [122]]]))

(defn char-alpha
  "Generates characters a -> z and A -> Z"
  ([]
   (with-leaf-interval
     (->> alpha-range
          (source/get-bytes *source*)
          (get-from-byte-buffer-abs .get)
          (core/char)))))

(defn long
  ([] (long Long/MIN_VALUE Long/MAX_VALUE))
  ([min] (long min Long/MAX_VALUE))
  ([floor ceiling]
   (with-leaf-interval
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling bytes/long->bytes)
          (source/get-bytes *source*)
          (get-from-byte-buffer-abs .getLong)))))

(def large-integer long)

(defn uuid [] (UUID. (long) (long)))

(defn float
  ([] (float (- Float/MAX_VALUE) Float/MAX_VALUE))
  ([min] (float min Float/MAX_VALUE))
  ([floor ceiling]
   (with-leaf-interval
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling (- Float/MIN_VALUE) bytes/float->bytes)
          (source/get-bytes *source*)
          (get-from-byte-buffer-abs .getFloat)))))

(def start-of-unreal-doubles (->> (range -1 -17 -1)
                                  (mapcat (fn [i] [[127 i] [-1 i]]))
                                  (set)))

(defn real-double
  ([] (real-double (- Double/MAX_VALUE) Double/MAX_VALUE))
  ([min] (real-double min Double/MAX_VALUE))
  ([floor ceiling]
   (with-leaf-interval
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling (- Double/MIN_VALUE) bytes/double->bytes)
          (bytes/punch-skip-values-out-of-ranges start-of-unreal-doubles)
          (source/get-bytes *source*)
          (get-from-byte-buffer-abs .getDouble)))))

(defn double
  ([] (real-double (- Double/MAX_VALUE) Double/MAX_VALUE))
  ([min] (real-double min Double/MAX_VALUE))
  ([floor ceiling]
   (with-leaf-interval
     (->> (bytes/split-number-line-min-max-into-bytewise-min-max floor ceiling (- Double/MIN_VALUE) bytes/double->bytes)
          (source/get-bytes *source*)
          (get-from-byte-buffer-abs .getDouble)))))

(def default-string-max-size 2048)

;TODO bias this so it's more likely to produce longer seqs.
(defn should-generate-elem? [floor ceiling len]
  (with-leaf-interval
    (<= 1 (let [value (byte 0 5)]                            ;Side-effecty
           (cond (< len floor) 1
                 (< ceiling (inc len)) 0
                 :default value)))))

(def default-collection-max-size 64)

(defmacro collection
  ([collection-init-fn generation-fn add-to-coll-fn min-size max-size]
   `(with-compound-interval
      (loop [result# (~collection-init-fn)]
        (let [next# (with-compound-interval-and-hints [[::proto/snippable nil]]
                                                      (if (should-generate-elem? ~min-size ~max-size (count result#))
                        (try
                          (~generation-fn)
                          (catch UniqueInputValuesExhaustedException e#
                            (if (and (< ~min-size (count result#))
                                     (< (count result#) ~max-size))
                              ::stop-collection-generation
                              (throw e#))))
                        ::stop-collection-generation))]
          (if-not (= ::stop-collection-generation next#)
            (recur (~add-to-coll-fn result# next#))
            result#))))))

(defn vec-of
  ([elem-gen] (vec-of elem-gen 0 default-collection-max-size))
  ([elem-gen size] (vec-of elem-gen size size))
  ([elem-gen min max] (collection vector elem-gen conj min max)))

(defn set-of
  ([elem-gen] (set-of elem-gen 0 default-collection-max-size))
  ([elem-gen size] (set-of elem-gen size size))
  ([elem-gen min max]
   (let [hint-id (swap! unique-hint-ids inc)]
     (collection hash-set
                 #(do (source/add-hints-to-next-interval *source* [[::proto/unique hint-id]])
                      (elem-gen))
                 conj
                 min
                 max))))

(defn string
  ([] (string 0 default-string-max-size))
  ([min] (string min (+ default-string-max-size min)))
  ([min max]
   (with-compound-interval
     (-> (vec-of char min max)
         (char-array)
         (String.)))))

(defn string-ascii
  ([] (string-ascii 0 default-string-max-size))
  ([min] (string-ascii min (+ default-string-max-size min)))
  ([min max]
   (with-compound-interval
     (-> (vec-of char-ascii min max)
         (char-array)
         (String.)))))

(defn string-alphanumeric
  ([] (string-alphanumeric 0 default-string-max-size))
  ([min] (string-alphanumeric min (+ default-string-max-size min)))
  ([min max]
   (with-compound-interval
     (-> (vec-of char-alphanumeric min max)
         (char-array)
         (String.)))))

(defn string-alpha
  ([] (string-alpha 0 default-string-max-size))
  ([min] (string-alpha min (+ default-string-max-size min)))
  ([min max]
   (with-compound-interval
     (-> (vec-of char-alpha min max)
         (char-array)
         (String.)))))

(defn elements
  "Pick a random value from the supplied collection. Shrinks to the first element of the collection."
  ([coll]
   (with-leaf-interval
     (let [target-idx (int 0 (dec (count coll)))]
       (nth (vec coll) target-idx)))))

(defn shuffle
  "Generates vectors with the elements of coll in random orders"
  ([coll]
    (with-compound-interval
      (if (empty? coll)
        coll
        (loop [remaining coll
               result []]
          (if-not (<= (count remaining) 1)
            (let [next-idx (int 0 (dec (count remaining)))]
              (recur (vec (concat (subvec remaining 0 next-idx) (subvec remaining (inc next-idx))))
                     (conj result (nth remaining next-idx))))
            (conj result (first remaining))))))))

(def char-symbol-special
  "Non-alphanumeric characters that can be in a symbol."
  [\* \+ \! \- \_ \?])

(defn frequency [frequencies-and-generators]
  (let [chosen (->> frequencies-and-generators
                    (sort-by (fn [[freq]] freq))              ;So we shrink towards the highest freq
                    (mapcat (fn [[freq gen]] (repeat freq gen)))
                    (elements))]
    (chosen)))

(defn char-symbol-initial []
  (frequency [[2 char-alpha]
              [1 (partial elements [\* \! \_ \?])]]))

(defn symbol-name-or-namespace []
  (->> #(frequency [[2 char-alphanumeric]
                    [1 (partial elements char-symbol-special)]])
       (vec-of)
       (cons (char-symbol-initial))
       (apply str)))

(defn keyword
  "Generate keywords without namespaces."
  []
  (with-compound-interval
    (frequency [[100 #(core/keyword (symbol-name-or-namespace))]
                [1 (constantly :/)]])))

(defn keyword-ns
  "Generate keywords with namespaces."
  []
  (with-compound-interval
    (core/keyword (symbol-name-or-namespace) (symbol-name-or-namespace))))

(defn symbol
  "Generate symbols without namespaces."
  []
  (with-compound-interval
    (frequency [[100 #(core/symbol (symbol-name-or-namespace))]
                [1 (constantly '/)]])))

(defn symbol-ns
  "Generate symbols with namespaces."
  []
  (with-compound-interval
    (core/symbol (symbol-name-or-namespace) (symbol-name-or-namespace))))

(defn ratio
  "Generates a `clojure.lang.Ratio`. Shrinks toward 0. Not all values generated
   will be ratios, as many values returned by `/` are not ratios."
  []
  (with-compound-interval
    (/ (int) (int Integer/MIN_VALUE -1 1 Integer/MAX_VALUE))))

(defn simple-type
  "Generates any non collection clojure value"
  []
  ((elements [int long double char string ratio boolean keyword keyword-ns symbol symbol-ns uuid])))

(defn simple-type-printable
  "Generates any printable non collection clojure value"
  []
  ((elements [int long double char-ascii string-ascii ratio boolean keyword keyword-ns symbol symbol-ns uuid])))

(defn map-of
  ([key-gen value-gen] (map-of key-gen value-gen 0 default-collection-max-size))
  ([key-gen value-gen size] (map-of key-gen value-gen size size))
  ([key-gen value-gen min-size max-size] (map-of key-gen value-gen min-size max-size {}))
  ([key-gen value-gen min-size max-size {:keys [value-gen-takes-key-as-arg]}]
   (with-compound-interval
     (let [hint-id (swap! unique-hint-ids inc)
           kv-gen #(let [k (with-compound-interval
                             (source/add-hints-to-next-interval *source* [[::proto/unique hint-id]])
                             (key-gen))
                         v (if value-gen-takes-key-as-arg
                             (value-gen k)
                             (value-gen))]
                     [k v])]
       (into {} (collection vector kv-gen conj min-size max-size))))))

(defn list-of
  ([gen] (list-of gen 0 default-collection-max-size))
  ([gen size] (list-of gen size size))
  ([gen min-size max-size] (collection (constantly '()) gen conj min-size max-size)))

(defn any* [limit leaf-gen]
  (if (< 0 (swap! limit dec))
    (frequency [[10 leaf-gen
                 1 (list-of leaf-gen)
                 1 (set-of leaf-gen)
                 1 (vec-of leaf-gen)
                 1 (map-of leaf-gen leaf-gen)]])
    (frequency [[3 (any* limit leaf-gen)
                 2 (list-of (any* limit leaf-gen))
                 2 (set-of (any* limit leaf-gen))
                 2 (vec-of (any* limit leaf-gen))
                 1 (map-of (any* limit leaf-gen) (any* limit leaf-gen))]])))

(defn any-printable []
  (any* (atom 200) simple-type-printable))

(defn any []
  (any* (atom 200) simple-type))

(defmacro defprop [name opts & body]
  (let [name-string (str name)
        opts (assoc opts ::test-name name-string)]
    (when-not (map? opts)
      (throw (IllegalArgumentException. "The second argument to defprop must be a map literal.")))
    `(t/deftest ~name
       (let [result# (run-prop ~opts (fn [] (do ~@body)))]
         (dorun (map t/report (get-in result# [::shrunk-results ::reported])))
         (when-let [message# (format-results ~name-string result# messages/clojure-seed-message (true? (:debug ~opts)))]
           (println message#))))))
