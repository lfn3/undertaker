(ns net.lfn3.undertaker.core
  (:refer-clojure :exclude [int byte long double short char float keyword boolean shuffle symbol list])
  (:require [clojure.core :as core]
            [clojure.test :as t]
            [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.source :as source]
            [net.lfn3.undertaker.source.fixed :as source.fixed]
            [net.lfn3.undertaker.shrink :as shrink]
            [net.lfn3.undertaker.messages :as messages]
            [net.lfn3.undertaker.source.wrapped-random :as source.random]
            [net.lfn3.undertaker.core-unbound :as unbound]
            [clojure.pprint])
  (:import (net.lfn3.undertaker UndertakerDebugException)
           (java.util UUID)))

(defonce seed-uniquifier* (volatile! (core/long 8682522807148012)))

(defn seed-uniquifier []
  (vswap! seed-uniquifier* #(unchecked-multiply (core/long %1) ;TODO: get rid of casts.
                                                (core/long 181783497276652981))))

(defn next-seed [seed]
  (bit-xor (seed-uniquifier) (inc seed)))

(def ^:dynamic *source* (net.lfn3.undertaker.source.wrapped-random/make-source (next-seed (System/nanoTime))))

(def unique-hint-ids (atom 0))

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
     (and (not (::proto/source-used? initial-results))
          (::result initial-results)) (str (messages/format-not-property-passed name results))
     (and (not (::proto/source-used? initial-results))
          (not (::result initial-results))) (str (messages/format-not-property-test-failed name results))
     (and (::proto/source-used? initial-results)
          (not (::result initial-results))) (str (messages/format-shrunk results) \newline (failed-lang-fn name results))
     debug? (str "\n\nDebug output follows:\n" (with-out-str (clojure.pprint/pprint results)))
     true (not-empty))))

;; === === === === === === === ===
;; Public api
;; === === === === === === === ===

(defn byte
  ([] (byte Byte/MIN_VALUE Byte/MAX_VALUE))
  ([min] (byte min Byte/MAX_VALUE))
  ([min max] (unbound/byte *source* min max)))

(defn boolean ([] (unbound/boolean *source*)))

(defn short
  ([] (short Short/MIN_VALUE Short/MAX_VALUE))
  ([min] (short min Short/MAX_VALUE))
  ([floor ceiling & more-ranges] (apply unbound/short *source* floor ceiling more-ranges)))

(defn nat [] (short 0 200))

(defn int
  ([] (int Integer/MIN_VALUE Integer/MAX_VALUE))
  ([min] (int min Integer/MAX_VALUE))
  ([floor ceiling & more-ranges] (apply unbound/int *source* floor ceiling more-ranges)))

(defn vector-ranges-to-byte-ranges [ranges]
  (vec (map (comp vec (partial map byte-array)) ranges)))

(def default-char-range (vector-ranges-to-byte-ranges [[[0x0000] [0xD800]]]))

(defn char
  "Returns a java primitive char. Does not generate values outside the BMP (Basic Multilingual Plane)."
  ([] (unbound/char *source* default-char-range)))

(def ascii-range (vector-ranges-to-byte-ranges [[[32] [126]]]))

(defn char-ascii
  "Generates printable ascii characters"
  ([] (unbound/char *source* ascii-range)))

(def alphanumeric-range (vector-ranges-to-byte-ranges [[[48] [57]]
                                                       [[65] [90]]
                                                       [[97] [122]]]))

(defn char-alphanumeric
  "Generates characters 0 -> 9, a -> z and A -> Z"
  ([] (unbound/char *source* alphanumeric-range)))

(def alpha-range (vector-ranges-to-byte-ranges [[[65] [90]]
                                                [[97] [122]]]))

(defn char-alpha
  "Generates characters a -> z and A -> Z"
  ([] (unbound/char *source* alpha-range)))

(defn long
  ([] (long Long/MIN_VALUE Long/MAX_VALUE))
  ([min] (long min Long/MAX_VALUE))
  ([floor ceiling & more-ranges] (apply unbound/long *source* floor ceiling more-ranges)))

(def large-integer long)

(defn uuid [] (UUID. (long) (long)))

(defn float
  ([] (float (- Float/MAX_VALUE) Float/MAX_VALUE))
  ([min] (float min Float/MAX_VALUE))
  ([floor ceiling & more-ranges] (apply unbound/float *source* floor ceiling more-ranges)))

(defn real-double
  ([] (real-double (- Double/MAX_VALUE) Double/MAX_VALUE))
  ([min] (real-double min Double/MAX_VALUE))
  ([floor ceiling & more-ranges] (apply unbound/real-double *source* floor ceiling more-ranges)))

(defn double
  ([] (double (- Double/MAX_VALUE) Double/MAX_VALUE))
  ([min] (double min Double/MAX_VALUE))
  ([floor ceiling & more-ranges] (apply unbound/double *source* floor ceiling more-ranges)))

(def default-string-max-size 2048)
(def default-collection-max-size 64)

(defn vec-of
  ([elem-gen] (vec-of elem-gen 0 default-collection-max-size))
  ([elem-gen size] (vec-of elem-gen size size))
  ([elem-gen min max] (unbound/collection *source* vector elem-gen conj min max)))

(defn set-of
  ([elem-gen] (set-of elem-gen 0 default-collection-max-size))
  ([elem-gen size] (set-of elem-gen size size))
  ([elem-gen min max]
   (let [hint-id (swap! unique-hint-ids inc)]
     (unbound/collection *source*
                         hash-set
                         #(do (source/add-hints-to-next-interval *source* [[::proto/unique hint-id]])
                              (elem-gen))
                         conj
                         min
                         max))))

(defn string
  ([] (string 0 default-string-max-size))
  ([min] (string min (+ default-string-max-size min)))
  ([min max] (unbound/string *source* default-char-range min max)))

(defn string-ascii
  ([] (string-ascii 0 default-string-max-size))
  ([min] (string-ascii min (+ default-string-max-size min)))
  ([min max] (unbound/string *source* ascii-range min max)))

(defn string-alphanumeric
  ([] (string-alphanumeric 0 default-string-max-size))
  ([min] (string-alphanumeric min (+ default-string-max-size min)))
  ([min max] (unbound/string *source* alphanumeric-range min max)))

(defn string-alpha
  ([] (string-alpha 0 default-string-max-size))
  ([min] (string-alpha min (+ default-string-max-size min)))
  ([min max] (unbound/string *source* alpha-range min max)))

(defn elements
  "Pick a random value from the supplied collection. Returns nil if the collection is empty.
  Shrinks to the first element of the collection."
  ([coll]
   (unbound/with-leaf-interval *source*
     (let [target-idx (unbound/int *source* 0 (dec (count coll)))]
       (nth (core/vec coll) target-idx nil)))))

(defn shuffle
  "Generates vectors with the elements of coll in random orders"
  ([coll]
    (unbound/with-compound-interval *source*
      (if (empty? coll)
        coll
        (loop [remaining coll
               result []]
          (if-not (<= (count remaining) 1)
            (let [next-idx (unbound/int *source* 0 (dec (count remaining)))]
              (recur (core/vec (concat (subvec remaining 0 next-idx) (subvec remaining (inc next-idx))))
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
  (unbound/with-compound-interval *source*
    (frequency [[100 #(core/keyword (symbol-name-or-namespace))]
                [1 (constantly :/)]])))

(defn keyword-ns
  "Generate keywords with namespaces."
  []
  (unbound/with-compound-interval *source*
    (core/keyword (symbol-name-or-namespace) (symbol-name-or-namespace))))

(defn symbol
  "Generate symbols without namespaces."
  []
  (unbound/with-compound-interval *source*
    (frequency [[100 #(core/symbol (symbol-name-or-namespace))]
                [1 (constantly '/)]])))

(defn symbol-ns
  "Generate symbols with namespaces."
  []
  (unbound/with-compound-interval *source*
    (core/symbol (symbol-name-or-namespace) (symbol-name-or-namespace))))

(defn ratio
  "Generates a `clojure.lang.Ratio`. Shrinks toward 0. Not all values generated
   will be ratios, as many values returned by `/` are not ratios."
  []
  (unbound/with-compound-interval *source*
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
   (unbound/with-compound-interval *source*
     (let [hint-id (swap! unique-hint-ids inc)
           kv-gen #(let [k (unbound/with-compound-interval *source*
                             (source/add-hints-to-next-interval *source* [[::proto/unique hint-id]])
                             (key-gen))
                         v (if value-gen-takes-key-as-arg
                             (value-gen k)
                             (value-gen))]
                     [k v])]
       (into {} (unbound/collection *source* vector kv-gen conj min-size max-size))))))

(defn list-of
  ([gen] (list-of gen 0 default-collection-max-size))
  ([gen size] (list-of gen size size))
  ([gen min-size max-size] (unbound/collection *source* (constantly '()) gen conj min-size max-size)))

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
