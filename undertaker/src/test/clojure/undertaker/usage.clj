(ns undertaker.usage
  (:require [clojure.test :refer [deftest is] :as t]
            [undertaker.core :as undertaker]))

(t/use-fixtures :each undertaker/fixture)

(deftest vector-coll-identity
  (undertaker/prop {:iterations 10}
    (let [actions (undertaker/vec-of (fn [source] (undertaker/from source #{#(conj %1 (undertaker/any))
                                                                            'pop})))]
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
