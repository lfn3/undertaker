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

#_(undertaker/defprop warns-if-no-generator-used {}
    (is true))

#_(undertaker/defprop warns-if-no-generator-used-and-fails {}
    (is false))

#_(undertaker/defprop shows-details-on-legit-fail {}
    (is (undertaker/bool)))

(undertaker/defprop double-full-range-get-test {}
  (let [value (undertaker/double)]
    (is (instance? Double value))
    (when (and (not= Double/NaN value)
               (Double/isFinite value))
      (is (>= value (- Double/MAX_VALUE)))
      (is (<= value Double/MAX_VALUE)))))

#_(undertaker/defprop double-around-one {}
  (let [value (undertaker/double-without-NaN -2.0 2.0)]
    (is (<= value 2.0))
    (is (>= value -2.0))))
