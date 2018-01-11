(ns net.lfn3.undertaker.usage-test
  (:require [clojure.test :refer [deftest is] :as t]
            [clojure.string :as str]
            [net.lfn3.undertaker.core :as undertaker]
            [orchestra.spec.test :as orchestra.test])
  (:import (net.lfn3.undertaker UniqueInputValuesExhaustedException)))

(t/use-fixtures :once #(do (orchestra.test/instrument)
                           (%1)
                           (orchestra.test/unstrument)))

(undertaker/defprop vector-coll-identity {}
  (let [actions (undertaker/vec-of (partial undertaker/elements #{#(conj %1 (undertaker/simple-type))
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
                      (is (undertaker/boolean)))

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

(undertaker/defprop int-in-one-of-many-ranges {}
  (let [i (undertaker/int 1 2 4 5 7 9)]
    (is (or (and (<= 1 i)
                 (<= i 2))
            (and (<= 4 i)
                 (<= i 5))
            (and (<= 7 i)
                 (<= i 9)))))
  (let [with--ve (undertaker/int -5 -3 -1 1 3 5)]
    (is (or (and (<= -5 with--ve)
                 (<= with--ve -3))
            (and (<= -1 with--ve)
                 (<= with--ve 1))
            (and (<= 3 with--ve)
                 (<= with--ve 5))))))

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

(undertaker/defprop can-get-ascii-string {}
  (let [s (undertaker/string-ascii)]
    (is (instance? String s))
    (is (every? ascii-chars s))))

(def alphanumeric-chars #{\A \a \B \b \C \c \D \d \E \e \F \f \G \g \H \h \I \i \J \j \K \k \L \l \M \m \N \n \O \o \0
                          \P \p \1 \Q \q \2 \R \r \3 \S \s \4 \T \t \5 \U \u \6 \V \v \7 \W \w \8 \X \x \9 \Y \y \Z \z})

(undertaker/defprop can-get-alphanumeric-chars {}
  (let [c (undertaker/char-alphanumeric)]
    (is (instance? Character c))
    (is (alphanumeric-chars c))))

(undertaker/defprop can-get-alphanumeric-string {}
  (let [s (undertaker/string-alphanumeric)]
    (is (instance? String s))
    (is (every? alphanumeric-chars s))))

(def alpha-chars #{\A \a \B \b \C \c \D \d \E \e \F \f \G \g \H \h \I \i \J \j \K \k \L \l \M \m \N \n \O \o \P \p \Q
                   \q \R \r \S \s \T \t \U \u \V \v \W \w \X \x \Y \y \Z \z})

(undertaker/defprop can-get-alpha-chars {}
  (let [c (undertaker/char-alpha)]
    (is (instance? Character c))
    (is (alpha-chars c))))

(undertaker/defprop can-get-alpha-string {}
  (let [s (undertaker/string-alpha)]
    (is (instance? String s))
    (is (every? alpha-chars s))))

(undertaker/defprop can-get-keyword {}
  (let [k (undertaker/keyword)]
    (is (keyword? k))
    (is (nil? (namespace k)))))

(def kv-pairs {:a :b
               \c \d
               "e" "f"
               1 2})

(undertaker/defprop can-get-map {}
  (let [m (undertaker/map-of (partial undertaker/elements (keys kv-pairs)) kv-pairs 0 3 {:value-gen-takes-key-as-arg true})]
    (is (map? m))
    (is (every? (set kv-pairs) m))))

(undertaker/defprop can-get-from-singleton-collection {}
  (let [coll [1]
        f (undertaker/elements coll)]
    (is (= 1 f))))

(undertaker/defprop can-get-set {}
  (let [s (undertaker/set-of (partial undertaker/int 0 5) 5 5)]
    (is (= (count s) 5))))

(undertaker/defprop can-get-a-uuid {}
  (let [u (undertaker/uuid)]
    (is (uuid? u))))

(undertaker/defprop can-shuffle {}
  (let [v (undertaker/vec-of undertaker/int)
        shuffled (undertaker/shuffle v)]
    (is (= (set v) (set shuffled)))))

(undertaker/defprop can-get-namespaced-keyword {}
  (let [k (undertaker/keyword-ns)
        stringified (str k)]
    (is (not= (name k) (str (rest stringified))))
    (is (namespace k))
    (str/includes? stringified "/")))

(undertaker/defprop can-get-symbol {}
  (let [s (undertaker/symbol)]
    (is (symbol? s))
    (is (nil? (namespace s)))))

(undertaker/defprop can-get-namespaced-symbol {}
  (let [s (undertaker/symbol-ns)
        stringified (str s)]
    (is (not= (name s) (str (rest stringified))))
    (is (namespace s))
    (str/includes? stringified "/")))

(undertaker/defprop can-get-ratio {}
  (let [r (undertaker/ratio)]
    (is r)))

(undertaker/defprop can-get-simple-type {}
  (let [s (undertaker/simple-type)]
    (is (not (coll? s)))))

(undertaker/defprop can-get-printable-simple-type {}
  (let [s (undertaker/simple-type-printable)]
    (is (not (coll? s)))
    (when (string? s)
      (is (every? ascii-chars s)))
    (when (char? s)
      (is (ascii-chars s)))))

(undertaker/defprop can-get-list {}
  (let [l (undertaker/list-of undertaker/keyword)]
    (is (every? keyword? l))
    (is (list? l) ))

  (let [sized (undertaker/list-of undertaker/long 10)]
    (is (= 10 (count sized)))))

(undertaker/defprop should-throw-when-input-gets-exhausted {}
  (is (thrown? UniqueInputValuesExhaustedException (undertaker/set-of (partial undertaker/int 1 3) 5 10))))
