(ns net.lfn3.undertaker.benchmarks
  (:require [net.lfn3.undertaker.source :as source]
            [net.lfn3.undertaker.core :as undertaker]))

(defn repeatedly-get-bytes-from-source [n source ranges]
  (doall (repeatedly n (partial source/get-bytes source ranges))))

(defn run-in-prop [n generator-fn]
  (undertaker/run-prop {:iterations n}
                       generator-fn))

(defn kv-gen [] (fn [] [(undertaker/keyword) (undertaker/keyword)]))

(defn string-kv-gen [] (fn [] [(undertaker/string) (undertaker/string)]))

(defn java-api-get-string [size-of-vector]
  (let [codepoint-gen (partial undertaker/int 48 57 65 90 97 122)]
    (->> (undertaker/vec-of codepoint-gen size-of-vector size-of-vector)
         (map char)
         (char-array)
         (String.))))
