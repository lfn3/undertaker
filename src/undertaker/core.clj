(ns undertaker.core
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [undertaker.proto :as proto])
  (:import (java.util Random)
           (java.nio ByteBuffer)))

(def ^:dynamic *source* nil)

(defn fixture [f]
  (with-bindings {#'*source* (Random.)}
    (f)))

(defn get-bytes [source number]
  (if (extends? proto/BytesSource (class source))
    (proto/get-bytes source number)
    (byte-array (repeatedly number (proto/get-byte source)))))

(extend-type Random
  proto/ByteSource
  (get-byte [this]
    (let [output (byte-array 1)]
      (.nextBytes this output)
      (aget output 0)))
  proto/BytesSource
  (get-bytes [this number]
    (let [output (byte-array number)]
      (.nextBytes this output)
      output)))

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
   (->> (get-bytes source number)
        (map (partial move-into-range min max))
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

(defn start-interval [name])

(defn end-interval [token])

(defmacro with-interval [name & body]
  `(let [interval-token# (start-interval ~name)
         result# (do ~@body)]
     (end-interval interval-token#)
     result#))

(defn int-gen
  ([] (int-gen *source*))
  ([source] (int-gen source Integer/MIN_VALUE))
  ([source min] (int-gen source Integer/MIN_VALUE Integer/MAX_VALUE))
  ([source min max]
   (with-interval (format-interval-name "int-gen" min max)
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
   (with-interval (format-interval-name "vec-gen" min max)
     (let [length (int-gen source min max)]
       (vec (repeatedly length #(elem-gen source)))))))

(defn bool-gen
  ([] (bool-gen *source*))
  ([source] (if (= 1 (take-byte source 0 1))
              true
              false)))

(s/fdef bool-gen
  :args (s/cat :source (s/? ::source))
  :ret boolean?)
