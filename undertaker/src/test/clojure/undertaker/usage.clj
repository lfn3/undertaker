(ns undertaker.usage
  (:require [clojure.test :refer [deftest is] :as t]
            [undertaker.core :as undertaker]))

(undertaker/defprop vector-coll-identity {}
    (let [actions (undertaker/vec-of (partial undertaker/from #{#(conj %1 (undertaker/any))
                                                                'pop}))]
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
