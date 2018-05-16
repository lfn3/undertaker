(ns net.lfn3.undertaker.messages
  (:require [clojure.stacktrace]))

(defn format-shrunk [{:keys [:net.lfn3.undertaker.core/shrunk-results]}]
  (let [{:keys [:net.lfn3.undertaker.core/cause]} shrunk-results]
    (format "Shrinking completed. The simplest values we could make the test fail with were:
%s

Once shrunk the cause of failure was:
%s"
            (vec (:net.lfn3.undertaker.core/generated-values shrunk-results))
            (if cause
              (with-out-str (clojure.stacktrace/print-cause-trace cause))
              "Unknown"))))

(defn clojure-seed-message [name {:keys [:net.lfn3.undertaker.core/seed]}]
  (format "You can add :seed to this test's options map to rerun this particular failing case:
(defprop %s {:seed %s} ...)"
          name seed))

(defn format-not-property-test-failed [name {:keys [:net.lfn3.undertaker.core/initial-results]}]
  (format "This test (%s) did not contain any calls to undertaker generators, so was not treated as a property test and repeatedly run or shrunk.
It failed due to:
%s"
          name,
          (:net.lfn3.undertaker.core/cause initial-results)))

(defn format-not-property-passed [name results]
  (format "%s did not contain any calls to undertaker generators, and so was not treated as a property test and run repeatedly.
You probably want to replace (defprop %s { opts... } test-body...) with (deftest %s test-body...)"
          name
          name
          name))

(defn format-initial-failure [name {:keys [:net.lfn3.undertaker.core/iterations-run
                                           :net.lfn3.undertaker.core/initial-results
                                           :net.lfn3.undertaker.core/seed]}]
  (let [{:keys [:net.lfn3.undertaker.core/cause]} initial-results]
    (format "%s failed after running %d times.

The cause of the failure was:
%s

The initial failing values were:
%s

The seed that generated the initial case was %s.

Shrinking in progress...
"
            name
            iterations-run
            (if (instance? Throwable cause)
              (str cause \newline
                (with-out-str (clojure.stacktrace/print-cause-trace cause)))
              cause)
            (vec (:net.lfn3.undertaker.core/generated-values initial-results))
            seed)))
