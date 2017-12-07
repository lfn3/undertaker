## Coming from [test.check](https://github.com/clojure/test.check/)

If you've used test.check in the past you're probably in a pretty good position.
One of my goals with this library was to make it as approachable as possible for people coming from
test.check, or just from writing regular [clojure.test](https://clojure.github.io/clojure/clojure.test-api.html) 
tests. Many of the functions are the same, and where they aren't hopefully it's because there's a simpler way to do
the same thing. 

The biggest difference is that test.check has special generator objects, while undertaker relies solely on
functions. In test.check use forms like `prop/for-all` to extract output from generators:

```clojure
(def property
  (prop/for-all [v (gen/vector gen/int)]
    (let [s (sort v)]
      (and (= (count v) (count s))
           (ascending? s)))))
```

In Undertaker you can just directly invoke a function:

```clojure
(undertaker/defprop property
  (let [v (undertaker/vec-of undertaker/int)
        s (sort v)]
        (is (= (count v) (count s)))
        (is (ascending? s))))
```

This also shows the other main difference, that Undertaker is designed to work with clojure.test as much 
possible. It obeys the same conventions, where `is` is used to perform assertions. Hopefully this means
converting a clojure.test test to an Undertaker property test is fairly simple:

```clojure
(deftest inc-should-add-one
  (is (= (inc 5) 6))
  (is (= (inc 2) 3)))
  
(undertaker/defprop inc-should-add-one ;replace deftest with defprop
  (let [i (undertaker/int)]            ;grab a value
  (is (= (inc i) (+ i 1)))))           ;assert
```

Okay, so that example is a bit contrived, especially since we have the `+` function. 
Usually the hard part is going to be the third step where you have to figure out what you can assert on
and how much confidence that actually gives you!

Thankfully people have already written lots about how find assertions for generative tests:
<!--TODO: links-->

### Changes to generators

The most obvious changes are the generators. A lot of them are based around java primitives, for instance
the `int` generator will only emit values representable as java primitive ints: -2147483648 to 2147483647.
`nat` and `large-integer` are still available if you don't need or want primitives.

I was able to remove many of the combinator generators, for example `fmap` can just be done with function 
application: 

```clojure
(def even-num-gen (gen/fmap (partial * 2) gen/nat))

; =

(defn even-num-gen [] (* 2 (undertaker/int)))
```

Other generators like `tuple` have been removed, since you can just use 'regular' clojure: 
`[(undertaker/int) (undertaker/string)]`. For the same reason `list-distinct`, `sorted-set` and etc are 
also gone: `(sort (undertaker/vec-of undertaker/int))`. `return` can be replaced with `constantly` from 
core.

### How the generation model differs

This is starting to dive into the internals of the library, but will offer some insight into why it behaves
the way it does.

<!--TODO: talk about set generation, etc -->