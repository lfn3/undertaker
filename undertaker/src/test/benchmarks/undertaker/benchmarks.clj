(ns undertaker.benchmarks
  (:require [undertaker.source :as source]
            [undertaker.core :as undertaker]))

(defn repeatedly-get-bytes-from-source [n source ranges]
  (doall (repeatedly n (partial source/get-bytes source ranges))))

(defn run-in-prop [n generator-fn]
  (undertaker.core/run-prop {:iterations n}
                            generator-fn))

(defn kv-gen [] (fn [] [(undertaker.core/keyword) (undertaker.core/keyword)]))

(defn string-kv-gen [] (fn [] [(undertaker.core/string) (undertaker.core/string)]))

(defn java-api-get-string [size-of-vector]
  (let [codepoint-gen (partial undertaker.core/int 48 57 65 90 97 122)]
    (->> (undertaker.core/vec-of codepoint-gen size-of-vector size-of-vector)
         (map char)
         (char-array)
         (String.))))
