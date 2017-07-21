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
            [clojure.test.check.generators :as gen]
            [undertaker.util :as util])
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

;TODO should be able to use (bit-and 0xff) and dec?
(defn move-towards-0 [byte]
  (cond
    (= 0 byte) byte
    (neg-int? byte) (inc byte)
    (pos-int? byte) (dec byte)))

(s/fdef move-towards-0
  :args (s/cat :byte ::util/byte)
  :ret ::util/byte
  :fn (fn [{:keys [args ret]}]
        (or (= 0 ret)
            (< (util/abs ret)
               (util/abs (:byte args))))))

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
       (map util/abs)
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

(defn byte-array-eq [arr0 arr1]
  (if-not (= (count arr0) (count arr1))
    false
    (loop [arr0 arr0
           arr1 arr1]
      (let [first-arr0 (first arr0)
            first-arr1 (first arr1)]
        (cond
          (nil? first-arr0) true                            ;Must have reached end
          (zero? (bit-xor first-arr0 first-arr1)) (recur (rest arr0) (rest arr1))
          :default false)))))

(s/fdef byte-array-eq
  :args (s/cat :arr1 bytes? :arr2 bytes?)
  :ret boolean?
  :fn (fn [{:keys [args ret]}]
        (= ret (= (vec (:arr0 args))
                  (vec (:arr1 args))))))

(defn unsigned [byte]
  (bit-and byte 0xff))

(s/fdef unsigned
  :args (s/cat :byte ::util/byte)
  :ret (s/and integer?
              (partial < 0)
              (partial > 256)))

;; We only care about failed shrinks that are less complex than the current shrunk-bytes.
(defn shrink
  ([bytes intervals fn]
   (if-not (empty? bytes)
     (loop [prev-source (fixed-source/make-fixed-source bytes intervals)
            failed-shrinks []]
       (let [shrunk-bytes (shrink-bytes (if (last failed-shrinks)
                                          (last failed-shrinks)
                                          (source/get-sourced-bytes prev-source)) intervals)
             shrunk-source (fixed-source/make-fixed-source shrunk-bytes intervals)
             continue? (can-shrink-more? shrunk-bytes)
             result-map (fn shrunk-source)
             passed? (true? (::result result-map))]
         ;(prn (map identity shrunk-bytes))
         ;(prn continue?)
         ;(prn passed?)
         ;(prn result-map)
         (cond
           (and continue? passed?) (recur prev-source (conj failed-shrinks shrunk-bytes))
           (and continue? (not passed?)) (recur shrunk-source [])
           passed? prev-source                              ;If the test hasn't failed, return last failing result.
           (not passed?) shrunk-source)))
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
            ::cause  e})
         (catch AssertionError e
           {::result false
            ::cause  e}))))

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

;; === === === === === === === ===
;; Public api
;; === === === === === === === ===

(defn bool
  ([] (bool *source*))
  ([source]
   (with-interval source (format-interval-name "bool")
     (if (= 1 (source/get-byte source 0 2))
       true
       false))))

(s/fdef bool
  :args (s/cat :source (s/? ::source/source))
  :ret boolean?)

(defn byte
  ([] (byte *source*))
  ([source] (byte source Byte/MIN_VALUE Byte/MAX_VALUE))
  ([source min] (byte source min Byte/MAX_VALUE))
  ([source min max]
   (with-interval source (format-interval-name "byte" min max)
     (source/get-byte source min max))))

(defn int
  ([] (int *source*))
  ([source] (int source Integer/MIN_VALUE Integer/MAX_VALUE))
  ([source min] (int source min Integer/MAX_VALUE))
  ([source floor ceiling]
   (with-interval source (format-interval-name "int" floor ceiling)
     (let [mins (util/get-bytes-from-int floor)
           maxes (util/get-bytes-from-int ceiling)
           first-genned (source/get-byte source (first mins) (first maxes))
           output-arr (byte-array 4)
           negative? (neg? first-genned)
           maxes (if (and negative? (not (neg? ceiling)))
                   (util/get-bytes-from-int (min -1 ceiling))   ;If we've already generated a -ve number, then the max is actually -1
                   maxes)
           mins (if (and (not negative?) (neg? floor))
                  (util/get-bytes-from-int (max 0 floor))     ;Conversely, if we've generated a +ve number, then the minimum is now zero.
                  mins)]
       (aset output-arr 0 first-genned)
       (loop [idx 1
              all-maxes? (is-max? first-genned 0 mins maxes)]
         (let [next-val (generate-next-byte-for-int source idx all-maxes? mins maxes)]
           (aset output-arr idx next-val)
           (when (< (inc idx) (count output-arr))
             (recur (inc idx)
                    (and all-maxes? (is-max? next-val idx mins maxes))))))
       (bytes->int output-arr)))))

(s/fdef int
  :args (s/cat :source (s/? ::source/source)
               :min (s/? int?)
               :max (s/? int?))
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Integer/MIN_VALUE
                      max Integer/MAX_VALUE}} args]
          (and                                              ;(<= min max)
            (<= min ret)
            (>= max ret)))))

(def default-max-size 64)

(defn vec-of
  ([elem-gen] (vec-of *source* elem-gen))
  ([source elem-gen] (vec-of source elem-gen 0))
  ([source elem-gen min] (vec-of source elem-gen min default-max-size))
  ([source elem-gen min max]
   (with-interval source (format-interval-name "vec" min max)
     (let [length (byte source min max)]
       (vec (repeatedly length #(elem-gen source)))))))

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
       (t/is (::result prop-result#) prop-result#))))

(defn fixture [f]
  (with-bindings {#'*source* (wrapped-random-source/make-source (System/nanoTime))}
    (f)))
