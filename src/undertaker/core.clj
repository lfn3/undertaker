(ns undertaker.core
  (:gen-class)
  (:require [clojure.core.async :as a]
            [clojure.spec :as s]
            [clojure.string :as str])
  (:import (java.util Random)
           (clojure.core.async.impl.protocols WritePort)
           (java.nio ByteBuffer)))

(defn tombstone?
  ([^Byte first-byte ^Byte second-byte] (= 0 (bit-xor first-byte second-byte))))

(defn tombstone-arr?
  ([byte-array] (tombstone? (first byte-array) (last byte-array))))

(s/def ::tombstone-candidate (s/and bytes? (comp (partial = 2) count)))

(s/fdef tombstone-arr?
  :args (s/cat :byte-array ::tombstone-candidate)
  :ret boolean?)

(defn byte? [b]
  (and (number? b)
       (>= Byte/MAX_VALUE b)
       (<= Byte/MIN_VALUE b)))

(s/fdef tombstone?
  :args (s/cat :first-byte byte? :second-byte byte?))

(s/def ::tombstone (s/and ::tombstone-candidate tombstone-arr?))

(defn take-til-tombstone-xf [xf]
  (let [last-byte (volatile! false)
        done? (volatile! false)]
    (fn
      ([] (xf))
      ([result]
       (let [prior @last-byte]
         (if (and (not @done?) prior)
           (do
             (vreset! last-byte false)
             (xf result prior))
           (xf result))))
      ([result input]
       (let [prior @last-byte]
         (cond
           @done? result
           (and prior (= input 0)) (do (vreset! done? true)
                                       result)
           (and (not prior) (= input 0)) (do
                                           (vreset! last-byte input)
                                           result)
           (and prior (not= 0 input)) (do (vreset! last-byte input)
                                          (xf result prior))
           (and (not prior) input) (xf result input)))))))

(defn take-bytes [source count]
  (.takeBytes source count))

(def test-generator-tree
  [{::name     "vec-gen [1 3]"
    ::data     [0 0 0 2]
    ::children [{::name "int-gen [0 1]"
                 ::data [0 0 0 0]}
                {::name "int-gen [0 1]"
                 ::data [0 0 0 1]}]}])

(defn can-shrink-more? [data]
  (not (every? (partial = 0) data)))

(defn shrink-data-halve [])

(defn make-split-fn []
  (let [found-nonzero? (volatile! false)]
    (fn [byte]
      (cond
        @found-nonzero? false
        (not= byte 0) (do (vreset! found-nonzero? true)
                          false)
        :default true))))

(defn shrink-data-step [data]
  (let [[prefix all] (split-with (make-split-fn) data)
        non-zero (drop (count prefix) all)]
    (concat prefix [(dec (first non-zero))] (rest non-zero))))

(s/fdef shrink-data-step
  :args (s/cat :data (partial every? byte?))
  :ret (s/and seq? (partial every? byte?))
  :fn (fn [{:keys [args ret]}] (<= (reduce + (:data args)) (reduce + ret))))

(defn shrink-walker [shrink-fn]
  (let [done? (volatile! false)]
    (fn [tree-part]
      (if (and (not @done?)
               (vector? tree-part)
               (= ::data (first tree-part))
               (can-shrink-more? (last tree-part)))
        (do (vreset! done? true)
            (prn "Shrinking " (last tree-part))
            [::data (shrink-fn (last tree-part))])
        tree-part))))

(defn shrink-tree [generator-tree]
  (clojure.walk/postwalk (shrink-walker shrink-data-step) test-generator-tree))

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
  ([source] (int-gen Integer/MIN_VALUE))
  ([source min] (int-gen Integer/MIN_VALUE Integer/MAX_VALUE))
  ([source min max]
   (with-interval (format-interval-name "int-gen" min max)
     (let [^ByteBuffer buffer (take-bytes source 4)]
       (+ min (mod (.getInt buffer) (- max min)))))))

(s/fdef int-gen
  :args (s/cat :source fn?
               :min int?
               :max int?)
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (and (<= (:min args) ret)
             (>= (:max args) ret))))

(defn vec-gen
  ([source] (vec-gen 0))
  ([source min] (vec-gen min Short/MAX_VALUE))
  ([source min max]
   (with-interval (format-interval-name "vec-gen" min max)
     (let [length (int-gen min max)]
       ))))
