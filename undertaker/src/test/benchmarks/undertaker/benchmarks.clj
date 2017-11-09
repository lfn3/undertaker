(ns undertaker.benchmarks
  (:require [undertaker.source :as source]
            [undertaker.core :as undertaker]))

(defn repeatedly-get-bytes-from-source [n source ranges]
  (doall (repeatedly n (partial source/get-bytes source ranges))))

(defn run-in-prop [n generator-fn]
  (undertaker.core/run-prop {:iterations n}
                            generator-fn))

(defn kv-gen [] (fn [] [(undertaker.core/keyword) (undertaker.core/keyword)]))
