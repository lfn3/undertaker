(ns undertaker.usage
  (:require [clojure.test :refer [deftest is] :as t]
            [undertaker.core :as undertaker]))

(t/use-fixtures :each undertaker/fixture)

(deftest vector-coll-identity
    (let [actions (undertaker/vec-of (fn [source] (undertaker/from #{#(conj %1 (undertaker/any))
                                                                     'pop})))]
      (when (seq? actions)
        (loop [action (first actions)
               remaining (rest actions)
               reference []
               under-test []]
          (let [updated-reference (action reference)
                updated-under-test (action under-test)]
            (is (= updated-reference updated-under-test))
            (when (seq remaining)
              (recur (first remaining)
                     (rest remaining)
                     updated-reference
                     updated-under-test)))))))
