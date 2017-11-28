(ns net.lfn3.undertaker.messages)

(def bug-tracker-url "https://github.com/lfn3/net.lfn3.undertaker.undertaker/issues/new")

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

(defn format-failed [name results]
  (format "%s failed after running %d times.

The simplest values we could make the test fail with were:
%s

The initial failing values were:
%s

The seed that generated the initial case was %s.
If you want to rerun this particular failing case, you can add this seed to the test.

If you're using Clojure, you can add :seed to this test's options map:
(defprop %s {:seed %s} ...)

If you're using Java and jUnit, you can add an annotation to the test:
@Test
@net.lfn3.undertaker.undertaker.junit.Seed(%s)
public void %s() { ... }"
          name
          (:net.lfn3.undertaker.core/iterations-run results)
          (vec (:net.lfn3.undertaker.core/shrunk-values results))
          (vec (:net.lfn3.undertaker.core/generated-values results))
          (:net.lfn3.undertaker.core/seed results)
          name
          (:net.lfn3.undertaker.core/seed results)
          (:net.lfn3.undertaker.core/seed results)
          name))

(defn format-not-property-test-failed [name results]
  (format "This test (%s) did not contain any calls to net.lfn3.undertaker.undertaker generators, so was not treated as a property test and repeatedly run or shrunk."
          name))

(defn format-not-property-passed [name results]
  (format "%s did not contain any calls to net.lfn3.undertaker.undertaker generators, and so was not treated as a property test and run repeatedly.
You probably want to replace (defprop %s { opts... } test-body...) with (deftest %s test-body...)"
          name
          name
          name))
