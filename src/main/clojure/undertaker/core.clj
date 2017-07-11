(ns undertaker.core
  (:gen-class)
  (:refer-clojure :exclude [int])
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [clojure.string :as str]
            [clojure.test :as t]
            [undertaker.proto :as proto]
            [undertaker.source :as source]
            [undertaker.source.wrapped-random :as wrapped-random-source]
            [undertaker.source.fixed :as fixed-source]
            [clojure.test.check.generators :as gen])
  (:import (java.util Random)
           (java.nio ByteBuffer)))

(defonce seed-uniquifier* (volatile! (long 8682522807148012)))

(defn seed-uniquifier []
  (vswap! seed-uniquifier* #(unchecked-multiply (long %1) (long 181783497276652981)))) ;TODO: get rid of casts.

(defn next-seed [seed]
  (bit-xor (seed-uniquifier) (inc seed)))

(s/fdef next-seed
  :args (s/cat :seed integer?)
  :ret integer?)

(def ^:dynamic *source* (wrapped-random-source/make-source (next-seed (System/nanoTime))))

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

(s/def ::byte (s/with-gen (s/and integer?
                                 (partial >= Byte/MAX_VALUE)
                                 (partial <= Byte/MIN_VALUE))
                          #(s/gen (set (range Byte/MIN_VALUE Byte/MAX_VALUE)))))

(s/fdef move-into-range
  :args (s/alt
          :byte (s/cat :byte ::byte
                       :min ::byte
                       :max ::byte)
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
  (str name " [" (str/join args " ") "]"))

(defmacro with-interval [source name & body]
  `(let [interval-token# (source/push-interval ~source ~name)
         result# (do ~@body)]
     (source/pop-interval ~source interval-token# result#)
     result#))

(defn check-result [result]
  (not-any? (comp (partial = :fail) :type) result))

(defn make-report-fn [an-atom]
  (fn [msg]
    (swap! an-atom conj msg)))

(defmacro run-and-report [source & body]
  `(let [result# (atom [])
         report-fn# (make-report-fn result#)]
     (with-bindings {#'t/report report-fn#
                     #'*source* ~source}
       (do ~@body))
     (check-result @result#)))

(defn move-towards-0 [byte]
  (cond
    (= 0 byte) byte
    (neg-int? byte) (inc byte)
    (pos-int? byte) (dec byte)))

(defn abs [i]
  (if (neg-int? i) (- i) i))

(s/fdef move-towards-0
  :args (s/cat :byte ::byte)
  :ret ::byte
  :fn (fn [{:keys [args ret]}]
        (or (= 0 ret)
            (< (abs ret)
               (abs (:byte args))))))

(defn shrink-bytes [bytes intervals]
  (let [already-zero (vec (take-while zero? bytes))
        not-yet-zero (drop-while zero? bytes)
        target (first not-yet-zero)]
    (byte-array
      (into (if target
              (conj already-zero (move-towards-0 target))
              already-zero)
            (rest not-yet-zero)))))

(defn sum-abs [coll]
  (->> coll
       (map abs)
       (reduce +)))

(s/fdef shrink-bytes
  :args (s/cat :byte-array bytes?
               :intervals (s/coll-of ::proto/interval))
  :ret bytes?
  :fn (fn [{:keys [args ret]}]
        (let [arg-bytes (:byte-array args)]
          (and (= (count arg-bytes)
                  (count ret))
               (>= (sum-abs arg-bytes)
                   (sum-abs ret))))))

;TODO feed this intervals, have it make decisions based on those.
(defn can-shrink-more? [bytes]
  (not-every? zero? bytes))

(s/fdef can-shrink-more?
  :args (s/cat :byte-array bytes?)
  :ret boolean?)

(defn shrink
  ([bytes intervals fn]
   (if-not (empty? bytes)
     (loop [prev-source (fixed-source/make-fixed-source bytes intervals)]
       (let [shrunk-bytes (shrink-bytes (source/get-sourced-bytes prev-source) intervals)
             shrunk-source (fixed-source/make-fixed-source shrunk-bytes intervals)]
         (if (true? (::result (fn shrunk-source)))
           prev-source                                      ;If the test hasn't failed, return last failing result.
           (if (can-shrink-more? shrunk-bytes)
             (recur shrunk-source)
             shrunk-source))))
     (fixed-source/make-fixed-source bytes intervals))))

(s/fdef shrink
  :args (s/cat :bytes bytes?
               :intervals (s/coll-of ::proto/interval)
               :fn fn?)
  :ret ::source/source)

(defn wrap-with-catch [f]
  (fn [source]
    (try {::result (f source)}
         (catch Exception e
           {::result false
            ::cause e})
         (catch AssertionError e
           {::result false
            ::cause e}))))

(defn run-prop-1 [source f]
  (let [f (wrap-with-catch f)
        result-map (-> (f source)
                       (assoc ::generated-values (map last (source/get-intervals source))))]
    (if (::result result-map)
      result-map
      (let [shrunk-source (shrink (source/get-sourced-bytes source) (source/get-intervals source) f)]
        (assoc result-map ::shrunk-values (map last (source/get-intervals shrunk-source)))))))

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
               :fn ::prop-fn)
  :ret ::results-map)

(defn run-prop
  ([{:keys [::seed ::iterations]
     :or   {seed       (bit-xor (System/nanoTime) (seed-uniquifier))
            iterations 1000}
     :as   opts-map}
    fn]
   (run-prop opts-map (wrapped-random-source/make-source seed) fn))
  ([{:keys [::seed ::iterations]
     :or   {iterations 1000}
     :as   opts-map}
    source
    fn]
   (loop [iterations-left iterations]
     (let [run-data (run-prop-1 source fn)]
       (if (and (-> run-data
                    ::result
                    (true?))
                (> iterations-left 0))
         (do
           (source/reset source)
           (recur (dec iterations-left)))
         (cond-> run-data
                 seed (assoc ::seed seed)))))))

(s/def ::iterations integer?)
(s/def ::prop-opts-map (s/keys :opt [::seed ::iterations]))

(s/fdef run-prop
  :args (s/cat :opts-map ::prop-opts-map
               :source (s/? ::source/source)
               :fn ::prop-fn)
  :ret ::results-map)

;; === === === === === === === ===
;; Public api
;; === === === === === === === ===

(defn int
  ([] (int *source*))
  ([source] (int source Integer/MIN_VALUE Integer/MAX_VALUE))
  ([source min] (int source min Integer/MAX_VALUE))
  ([source min max]
   (with-interval source (format-interval-name "int" min max)
     (let [^ByteBuffer buffer (ByteBuffer/wrap (source/get-bytes source 4))]
       (move-into-range (.getInt buffer) min max Integer/MIN_VALUE Integer/MAX_VALUE)))))

(s/fdef int
  :args (s/cat :source (s/? ::source/source)
               :min (s/? int?)
               :max (s/? int?))
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Integer/MIN_VALUE
                      max Integer/MAX_VALUE}} args]
          (and (<= min ret)
               (>= max ret)))))

(def default-max-size 64)

(defn vec-of
  ([elem-gen] (vec-of *source* elem-gen))
  ([source elem-gen] (vec-of source elem-gen 0))
  ([source elem-gen min] (vec-of source elem-gen min default-max-size))
  ([source elem-gen min max]
   (with-interval source (format-interval-name "vec" min max)
     (let [length (int source min max)]
       (vec (repeatedly length #(elem-gen source)))))))

(defn bool
  ([] (bool *source*))
  ([source]
   (with-interval source (format-interval-name "bool")
     (if (= 1 (-> (source/get-byte source)
                  (move-into-range 0 1)))
       true
       false))))

(s/fdef bool
  :args (s/cat :source (s/? ::source/source))
  :ret boolean?)

(defn from
  ([coll] (from *source* coll))
  ([source coll]
   (with-interval source (format-interval-name "from" coll)
     (let [target-idx (int source 0 (dec (count coll)))]
       (nth (vec coll) target-idx)))))

(s/fdef from
  :args (s/cat :source (s/? ::source) :coll (s/coll-of any?))
  :ret any?
  :fn (fn [{:keys [args ret]}] (contains? (set (:coll args)) ret)))

(def any-gens #{bool
                int})

(defn any
  ([] (any *source*))
  ([source] (any source #{}))
  ([source exclusions]
   (with-interval source (format-interval-name "any")
     (let [chosen-generator (from (remove exclusions any-gens))]
       (chosen-generator source)))))

(s/fdef any
  :args (s/cat :source (s/? ::source)
               :exclusions (s/or :fn (s/fspec :args (s/cat :item any?)
                                              :ret boolean?)
                                 :set set?))
  :ret any?)

(defmacro prop
  [opts & body]
  `(let [result# (atom [])
         report-fn# (make-report-fn result#)
         wrapped-body# (fn [source#] (run-and-report source# ~@body))]
     (run-prop ~opts wrapped-body#)))

(defmacro defprop
  [name opts & body]
  `(t/deftest ~name
     (let [prop-result# (prop ~opts ~@body)]
       (t/is (:result prop-result#) prop-result#))))

(defn fixture [f]
  (with-bindings {#'*source* (wrapped-random-source/make-source (System/nanoTime))}
    (f)))
