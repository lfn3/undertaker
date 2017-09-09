(ns undertaker.usage
  (:require [clojure.test :refer [deftest is] :as t]
            [undertaker.core :as undertaker]
            [orchestra.spec.test :as orchestra.test]))

(t/use-fixtures :once #(do (orchestra.test/instrument)
                           (%1)
                           (orchestra.test/unstrument)))

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

(undertaker/defprop real-double {}
  (let [real-double (undertaker/real-double)]
    (is (not (Double/isNaN real-double)))
    (is (Double/isFinite real-double))))

(undertaker/defprop double-around-one {}
  (let [value (undertaker/real-double -1.0 1.0)]
    (is (<= value 1.0))
    (is (>= value -1.0))))

(undertaker/defprop double-above-one {}
  (let [double (undertaker/real-double 1.0)]
    (is (<= 1.0 double))))

(undertaker/defprop short-above-one {}
  (let [short (undertaker/short 1)]
    (is (<= 1 short))))

(undertaker/defprop int-above-one {}
  (let [int (undertaker/int 1)]
    (is (<= 1 int))))

(undertaker/defprop can-get-a-string-of-length-1 {}
  (let [string (undertaker/string 1 1)]
    (is (instance? String string))
    (is (= 1 (count string)))))

(def ascii-chars #{\space \@ \` \! \A \a \" \B \b \# \C \c \$ \D \d \% \E \e \& \F \f \' \G \g \( \H \h \) \I \i \* \J
                   \j \+ \K \k \, \L \l \- \M \m \. \N \n \/ \O \o \0 \P \p \1 \Q \q \2 \R \r \3 \S \s \4 \T \t \5 \U
                   \u \6 \V \v \7 \W \w \8 \X \x \9 \Y \y \: \Z \z \; \[ \{ \< \\ \| \= \] \} \> \^ \~ \? \_})

(undertaker/defprop can-get-ascii-chars {}
  (let [c (undertaker/char-ascii)]
    (is (instance? Character c))
    (is (ascii-chars c))))

(def alphanumeric-chars #{\A \a \B \b \C \c \D \d \E \e \F \f \G \g \H \h \I \i \J \j \K \k \L \l \M \m \N \n \O \o \0
                          \P \p \1 \Q \q \2 \R \r \3 \S \s \4 \T \t \5 \U \u \6 \V \v \7 \W \w \8 \X \x \9 \Y \y \Z \z})

(undertaker/defprop can-get-alphanumeric-chars {}
  (let [c (undertaker/char-alphanumeric)]
    (is (instance? Character c))
    (is (alphanumeric-chars c))))
