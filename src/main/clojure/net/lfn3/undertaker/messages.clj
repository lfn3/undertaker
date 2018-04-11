(ns net.lfn3.undertaker.messages)

(def bug-tracker-url "https://github.com/lfn3/undertaker/issues/new")

(defn missing-source-err-msg []
  "Source is nil.
This probably means you're missing part of the test setup.

If you're using Clojure:
  Switch this deftest for:
    (defprop test-name {}
      test-body...)

If you're using Java and jUnit:
  Add the junit test rule to the top of your file:
    @Rule
    public Source source = new SourceRule();")

(defn ^String more-than-one-source-in-test-scope-err-msg []
  (str "There's more than one source being used in this test.
This probably means you've manually supplied a source in a generator.
You should be using the automatically managed net.lfn3.undertaker.core/*source*.

It might also mean you've found a bug, if so, please report it at " bug-tracker-url))

(defn ^String non-fixed-source-during-shrinking-error-msg []
  (str "The source used during shrinking was not a fixed source.
This is most likely a bug in Undertaker, please report it at " bug-tracker-url))

(defn ^String already-bound-source-error-string []
  (str
    "The *source* var has already been set, and something is trying to bind another value to it.
This probably means you've nested tests inside each other.

If you can't find the cause of the error, please raise an issue at "
    bug-tracker-url))

(defn format-shrunk [{:keys [:net.lfn3.undertaker.core/shrunk-results]}]
  (format "Shrinking completed. The simplest values we could make the test fail with were:
%s

Once shrunk the cause of failure was:
%s"
          (vec (:net.lfn3.undertaker.core/generated-values shrunk-results))
          (:net.lfn3.undertaker.core/cause shrunk-results)))

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
          (:net.lfn3.undertaker.core/cause initial-results)
          (vec (:net.lfn3.undertaker.core/generated-values initial-results))
          seed))
