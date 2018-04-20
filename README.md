# Undertaker

![The undertaker emerging from it's coffin](http://lfn3.net/img/projects/undertaker/undertaker.gif)

A property testing library for Clojure and [Java](https://github.com/lfn3/undertaker-junit).
If you've used [test.check](https://github.com/clojure/test.check) before you might want to start by reading
[Coming from test.check](https://github.com/lfn3/undertaker/blob/master/docs/coming-from-test-check.md) instead of this.
Otherwise, if you're new to property based testing in general, or property testing in Clojure, read on.

## What is property based testing?

Property based testing is based around invariants: things that will always be true about the results of running your 
code, regardless of the inputs to that code. That's fairly abstract, so lets look at an example:

```clojure
(undertaker/defprop sum-should-be-greater-than-inputs {}
  (let [a (undertaker/int)
        b (undertaker/int)])
    (is (< a (+ a b)))
    (is (< b (+ a b))))
```

So this is a very simple property based test that checks when we add two numbers that the result is greater each of 
those numbers. This is a perfect example of the value of property testing, since at first glance it seems like this 
test should pass, however it will fail if for instance `a = 0` and `b = -1`. Typically Undertaker will not generate
this particular test case, but rather one with much more complicated test inputs such as `a = -245645` and `b = -12313`.
Once a failing test case is located, Undertaker will attempt to shrink, or reduce the complexity of the inputs to be
as near to zero as possible.

## How is Undertaker different from other property testing libraries?

Unlike other property testing libraries such as test.check, Undertaker allows to generate input for the test at any 
point during the test, rather than having to specify all the input up front. I think this makes it much easier to write
tests, especially since it makes the generation process more like regular Clojure code.

## How mature is this libary?

It's been used at [LMAX](https://www.lmax.com/) via [undertaker-junit](https://github.com/lfn3/undertaker-junit) for 
several months. It's still under heavy and active development, and will continue to be so for the foreseeable future.
API changes *should* be minimal and non-breaking, but of course, I don't want to make any guarantees about that until
v1.0, and it's seen some use outside of LMAX.

At the moment the documentation is still fairly slim. Improving the documentation and the performance are my main focus
at present. In the interim, the best guide for how to use the API is probably the usage tests 
[here](https://github.com/lfn3/undertaker/blob/master/src/test/clojure/net/lfn3/undertaker/usage_test.clj)

## Acknowledgements

[David R. MacIver](https://www.drmaciver.com/) for writing [Hypothesis](https://hypothesis.works/) and 
[Hypothesis-Java](https://github.com/HypothesisWorks/hypothesis-java) without which there is approximately zero chance 
I would have realized there was a better way of doing property testing.

[Reid Draper](https://twitter.com/reiddraper), [Gary Fredericks](https://twitter.com/gfredericks_) and all of the
[contributors](https://github.com/clojure/test.check/graphs/contributors) to 
[test.check](https://github.com/clojure/test.check/). test.check was where I started with property testing, and the 
place I "borrowed" big chunks of Undertaker's api from.

My employer, [LMAX](https://www.lmax.com/) for paying for the significant amount time I've spent working on this thing.
In particular [Mike Barker](https://twitter.com/mikeb2701) for doing significant rubber duck duty.

## License

Copyright © 2017 Liam Falconer

Distributed under the Apache License, Version 2.0
