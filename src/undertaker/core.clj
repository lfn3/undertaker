(ns undertaker.core
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as s.gen]
            [clojure.string :as str]
            [undertaker.proto :as proto]
            [clojure.test :as t]
            [undertaker.source :as source])
  (:import (java.util Random)
           (java.nio ByteBuffer)))

(def ^:dynamic *source* nil)

(defn fixture [f]
  (with-bindings {#'*source* (source/make-source (System/nanoTime))}
    (f)))

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

(defn byte? [b]
  (and (number? b)
       (integer? b)
       (<= b Byte/MAX_VALUE)
       (>= b Byte/MIN_VALUE)))

(s/def ::byte byte?)
;TODO should be
#_(s/with-gen byte?
              #(s.gen/fmap first (s.gen/bytes)))
;But that's broke. Not sure why, yet.

(s/fdef move-into-range
  :args (s/alt
          :bytes
          (s/cat :byte ::byte
                 :min ::byte
                 :max ::byte)
          :any-integers
          (s/cat :byte integer?
                 :min integer?
                 :max integer?
                 :type-min integer?
                 :type-max integer?))
  :ret integer?
  :fn (fn [{:keys [args ret]}]
        (<= (:max args) ret)
        (>= (:max args) ret)))

(defn take-byte
  ([source] (take-byte source Byte/MIN_VALUE Byte/MAX_VALUE))
  ([source ^Byte max] (take-byte source Byte/MIN_VALUE max))
  ([source ^Byte min ^Byte max]
   (let [raw (proto/get-byte source)]
     (if-not (and (= Byte/MIN_VALUE min) (= Byte/MAX_VALUE max))
       (move-into-range raw min max)
       raw))))

(s/def ::source (comp (partial extends? proto/ByteSource) class))

(s/fdef take-byte
  :args (s/cat :source ::source
               :min (s/? ::byte)
               :max (s/? ::byte))
  :ret ::byte
  :fn (fn [{:keys [args ret]}]
        (<= (:max args) ret)
        (>= (:max args) ret)))

(defn take-bytes
  ([source number] (take-bytes source Byte/MIN_VALUE Byte/MAX_VALUE number))
  ([source max number] (take-bytes source Byte/MIN_VALUE max number))
  ([source ^Byte min ^Byte max number]
   (->> (proto/get-bytes source number)
        (map #(move-into-range %1 min max))
        (byte-array))))

(s/fdef take-bytes
  :args (s/cat :source ::source
               :min (s/? ::byte)
               :max (s/? ::byte)
               :number integer?)
  :ret bytes?
  :fn (fn [{:keys [args ret]}]
        (= (:number args) (count ret))
        (<= (:min args) (reduce min ret))
        (>= (:max args) (reduce max ret))))

(defn format-interval-name [name & args]
  (str name " [" (str/join args " ") "]"))

(defmacro with-interval [source name & body]
  `(let [interval-token# (proto/push-interval ~source ~name)
         result# (do ~@body)]
     (proto/pop-interval ~source interval-token# result#)
     result#))

(def fixed-int-offset (- Integer/MIN_VALUE))

(defn int-gen
  ([] (int-gen *source*))
  ([source] (int-gen source Integer/MIN_VALUE Integer/MAX_VALUE))
  ([source min] (int-gen source min Integer/MAX_VALUE))
  ([source min max]
   (with-interval source (format-interval-name "int-gen" min max)
     (let [^ByteBuffer buffer (ByteBuffer/wrap (take-bytes source 4))]
       (move-into-range (.getInt buffer) min max Integer/MIN_VALUE Integer/MAX_VALUE)
       ; (+ min (mod adjusted-int (- max min)))
       ))))

(s/fdef int-gen
  :args (s/cat :source (s/? ::source)
               :min (s/? int?)
               :max (s/? int?))
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (and (<= (:min args) ret)
             (>= (:max args) ret))))

(def default-max-size 64)

(defn vec-gen
  ([elem-gen] (vec-gen *source* elem-gen))
  ([source elem-gen] (vec-gen source elem-gen 0))
  ([source elem-gen min] (vec-gen source elem-gen min default-max-size))
  ([source elem-gen min max]
   (with-interval source (format-interval-name "vec-gen" min max)
     (let [length (int-gen source min max)]
       (vec (repeatedly length #(elem-gen source)))))))

(defn bool-gen
  ([] (bool-gen *source*))
  ([source]
   (with-interval source (format-interval-name "bool-gen")
     (if (= 1 (take-byte source 0 1))
       true
       false))))

(s/fdef bool-gen
  :args (s/cat :source (s/? ::source))
  :ret boolean?)

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

(defonce seed-uniquifier* (volatile! (long 8682522807148012)))

(defn seed-uniquifier []
  (vswap! seed-uniquifier* #(unchecked-multiply (long %1) (long 181783497276652981)))) ;TODO: get rid of casts.

(defn next-seed [seed]
  (bit-xor (seed-uniquifier) (inc seed)))

(defn move-towards-0 [byte]
  (cond
    (= 0 byte) byte
    (neg-int? byte) (inc byte)
    (pos-int? byte) (dec byte)))

(s/fdef move-towards-0
  :args (s/cat :byte ::byte)
  :ret ::byte
  :fn (fn [{:keys [args ret]}]
        (or (= 0 ret)
            (< (Math/abs ret)
               (Math/abs (:byte args))))))

(defn shrink-bytes [bytes intervals]
  (let [already-zero (take-while (partial = 0) bytes)
        not-yet-zero (drop-while (partial = 0) bytes)]
    (concat already-zero
            (some-> (first not-yet-zero)
                    (move-towards-0)
                    vector)
            (rest not-yet-zero))))

(s/fdef shrink-bytes
  :args (s/cat :bytes (s/coll-of ::byte)
               :intervals (s/coll-of ::source/interval))
  :ret (s/coll-of byte?))

;TODO feed this intervals, have it make decisions based on those.
(defn can-shrink-more? [bytes]
  (not-every? zero? bytes))

(s/fdef can-shrink-more?
  :args (s/cat :bytes (s/coll-of byte?))
  :ret boolean?)

(defn shrink
  ([bytes intervals fn]
   (if-not (empty? bytes)
     (loop [shrunk-bytes (shrink-bytes bytes intervals)]
       (let [source (source/make-fixed-source shrunk-bytes intervals)]
         (if (and (false? (fn source))
                  (can-shrink-more? shrunk-bytes))
           (recur (shrink-bytes shrunk-bytes intervals))
           source)))
     (source/make-fixed-source bytes intervals))))

(defn run-prop-1 [source fn]
  (if (fn source)
    {:result           true
     :generated-values (map last (proto/get-intervals source))}
    (let [shrunk-source (shrink (proto/freeze source) (proto/get-intervals source) fn)]
      {:result           false
       :generated-values (map last (proto/get-intervals source))
       :shrunk-values    (map last (proto/get-intervals shrunk-source))})))

(s/fdef run-prop-1
  :args (s/cat :source (partial satisfies? proto/Recall)
               :fn fn?)
  :ret boolean?)

(defn run-prop [{:keys [seed iterations]
                 :or   {seed       (System/nanoTime)
                        iterations 1000}
                 :as   opts-map}
                fn]
  (loop [iterations-left iterations
         seed (bit-xor seed (seed-uniquifier))]
    (if (> iterations-left 0)
      (let [source (source/make-source seed)
            run-data (run-prop-1 source fn)]
        (if (-> run-data
                :result
                (true?))
          (recur
            (dec iterations-left)
            (next-seed seed))
          run-data))
      true)))

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
