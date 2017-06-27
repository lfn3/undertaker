(ns undertaker.core
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
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

;Due to bytes being signed.
(def fixed-byte-offset 128)

(defn move-into-range [byte min max]
  (let [byte-range (- Byte/MAX_VALUE Byte/MIN_VALUE)
        range (- max min)]
    (if (= range 0)
      min
      (let [divisor (/ byte-range range)
            adjusted-byte (+ fixed-byte-offset byte)]
        (Math/round (double (+ min (/ adjusted-byte divisor))))))))

(defn byte? [b]
  (and (number? b)
       (integer? b)
       (<= b Byte/MAX_VALUE)
       (>= b Byte/MIN_VALUE)))

(s/fdef move-into-range
  :args (s/cat :byte (s/? byte?)
               :min (s/? byte?)
               :max (s/? byte?))
  :ret byte?
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
               :min (s/? byte?)
               :max (s/? byte?))
  :ret byte?
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
               :min (s/? byte?)
               :max (s/? byte?)
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
     (proto/pop-interval ~source interval-token#)
     result#))

(defn int-gen
  ([] (int-gen *source*))
  ([source] (int-gen source Integer/MIN_VALUE Integer/MAX_VALUE))
  ([source min] (int-gen source min Integer/MAX_VALUE))
  ([source min max]
   (with-interval source (format-interval-name "int-gen" min max)
     (let [^ByteBuffer buffer (ByteBuffer/wrap (take-bytes source 4))]
       (+ min (mod (.getInt buffer) (- max min)))))))

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

(defn shrink [bytes fn]
  (prn "Shrinking..."))

(defn run-prop-1 [source fn]
  (if (fn source)
    true
    (do (shrink (proto/freeze source) fn)
        false)))

(defn run-prop [{:keys [seed iterations]
                 :or   {seed       (System/nanoTime)
                        iterations 1000}
                 :as   opts-map}
                fn]
  (loop [iterations-left iterations
         seed (bit-xor seed (seed-uniquifier))]
    (if (> iterations-left 0)
      (let [source (source/make-source seed)]
        (if (run-prop-1 source fn)
          (recur
            (dec iterations-left)
            (next-seed seed))
          false))
      true)))

(defmacro prop
  [opts & body]
  `(let [result# (atom [])
         report-fn# (make-report-fn result#)
         wrapped-body# (fn [source#] (run-and-report source# ~@body))]
     (run-prop ~opts wrapped-body#)))
