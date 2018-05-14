(ns net.lfn3.undertaker.specs.core
  (:require [net.lfn3.undertaker.core :as undertaker]
            [net.lfn3.undertaker.source :as source]
            [net.lfn3.undertaker.specs.proto]
            [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.specs.source]
            [clojure.test.check.generators :as gen]))

(s/fdef undertaker/next-seed
        :args (s/cat :seed integer?)
        :ret integer?)

(s/def ::undertaker/result boolean?)
(s/def ::undertaker/generated-values (s/coll-of any?))
(s/def ::undertaker/seed integer?)
(s/def ::undertaker/iterations-run integer?)
(s/def ::undertaker/results-map (s/keys :req [::undertaker/result ::proto/source-used?]
                                        :opt [::undertaker/generated-values]))

(s/def ::undertaker/cause (s/nilable any?))
(s/def ::undertaker/reported (s/coll-of any?))

(s/def ::undertaker/prop-fn-results (s/keys :req [::undertaker/result ::undertaker/cause ::undertaker/reported]))

(s/def ::undertaker/prop-fn (s/with-gen fn?
                                        #(gen/return (fn []))))
#_(s/def ::undertaker/prop-fn (s/fspec :args (s/cat)    ;I think these specs causes issues since we invoke them during test runs.
                                       :ret any?))
(s/def ::undertaker/wrapped-prop-fn (s/with-gen fn?
                                                #(gen/fmap constantly (s/gen ::undertaker/prop-fn-results))))
#_(s/def ::undertaker/wrapped-prop-fn (s/fspec :args (s/cat :source ::source/source)
                                               :ret ::undertaker/prop-fn-results))

(s/def ::undertaker/initial-results ::undertaker/results-map)
(s/def ::undertaker/shrunk-results ::undertaker/results-map)
(s/def ::undertaker/all-results (s/keys :req [::undertaker/initial-results]
                                        :opt [::undertaker/shrunk-results ::undertaker/seed ::undertaker/iterations-run]))

(s/fdef undertaker/wrap-fn
  :args (s/cat :fn ::undertaker/prop-fn)
  :ret ::undertaker/wrapped-prop-fn)

(s/def ::undertaker/iterations integer?)
(s/def ::undertaker/test-name string?)
(s/def ::undertaker/prop-opts-map (s/keys :req [::undertaker/test-name]
                                          :opt-un [::undertaker/seed ::undertaker/iterations]))
(s/def ::undertaker/disallowed-values (s/coll-of ::bytes/bytes))

(s/fdef undertaker/run-prop
  :args (s/cat :opts-map ::undertaker/prop-opts-map
               :fn ::undertaker/prop-fn)
  :ret ::undertaker/all-results)

(s/fdef undertaker/format-results
  :args (s/cat :name string?
               :results ::undertaker/all-results
               :failed-lang-fn (s/with-gen fn?
                                           #(gen/fmap constantly (s/gen string?)))
               :debug boolean?)
  :ret (s/nilable string?))

(s/fdef undertaker/byte
  :args (s/and (s/cat :min (s/? ::bytes/byte) :max (s/? ::bytes/byte))
               (fn [{:keys [min max]
                     :or {min Byte/MIN_VALUE
                          max Byte/MAX_VALUE}}] (<= min max)))
  :ret ::bytes/byte
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Byte/MIN_VALUE
                      max Byte/MAX_VALUE}} args]
          (and (<= min ret)
               (<= ret max)))))

(s/fdef undertaker/boolean
  :args (s/cat)
  :ret boolean?)

(s/def ::short
  (s/and int?
       #(<= Short/MIN_VALUE %1)
       #(<= %1 Short/MAX_VALUE)))

(s/fdef undertaker/short
  :args (s/and (s/cat :min (s/? ::short)
                      :max (s/? ::short))
               (fn [{:keys [min max]
                     :or {min Short/MIN_VALUE
                          max Short/MAX_VALUE}}] (<= min max)))
  :ret ::short
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Short/MIN_VALUE
                      max Short/MAX_VALUE}} args]
          (and (<= min ret)
               (<= ret max)))))

(s/fdef undertaker/int
  :args (s/and (s/cat :floor (s/? int?)
                      :ceiling (s/? int?)
                      :more-ranges (s/* int?))
               (fn [{:keys [floor ceiling more-ranges]
                     :or {floor       Integer/MIN_VALUE
                          ceiling     Integer/MAX_VALUE
                          more-ranges []}}]
                 (and (<= floor ceiling)
                      (even? (count more-ranges)))))
  :ret int?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [floor ceiling more-ranges]
               :or   {floor       Integer/MIN_VALUE
                      ceiling     Integer/MAX_VALUE
                      more-ranges []}} args
              total-floor (apply min floor more-ranges)
              total-ceiling (apply max ceiling more-ranges)]
          (and (<= total-floor ret)
               (>= total-ceiling ret)
               (even? (count more-ranges))))))

(s/fdef undertaker/char
  :args (s/cat)
  :ret char?)

(s/fdef undertaker/char-ascii
  :args (s/cat)
  :ret char?)                                               ;TODO: check against set in net.lfn3.undertaker.undertaker.usage. Need to move it to a different ns first though.

(s/fdef undertaker/char-alphanumeric
  :args (s/cat)
  :ret char?)                                               ;TODO: check against set in net.lfn3.undertaker.undertaker.usage. Need to move it to a different ns first though.

(s/fdef undertaker/char-alpha
  :args (s/cat)
  :ret char?)                                               ;TODO: check against set in net.lfn3.undertaker.undertaker.usage. Need to move it to a different ns first though.

(s/def ::long
  (s/and int?
       #(<= Long/MIN_VALUE %1)
       #(<= %1 Long/MAX_VALUE)))

(s/fdef undertaker/long
  :args (s/and (s/cat :min (s/? ::long)
                      :max (s/? ::long))
               (fn [{:keys [min max]
                     :or {min Long/MIN_VALUE
                          max Long/MAX_VALUE}}] (<= min max)))
  :ret ::long
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min Long/MIN_VALUE
                      max Long/MAX_VALUE}} args]
          (and (<= min ret)
               (<= ret max)))))

(s/def ::float
  (s/with-gen (s/and float?
                     #(or (.isNaN %1) (<= %1 Float/MAX_VALUE))
                     #(or (.isNaN %1) (<= (float (- Float/MAX_VALUE))) %1)
                     (partial not= -0.0))
              #(gen/fmap unchecked-float (gen/double* {:min (float (- Float/MAX_VALUE))
                                                       :max Float/MAX_VALUE
                                                       :NaN false}))))

(s/fdef undertaker/float
  :args (s/and (s/cat :floor (s/? ::float)
                      :ceiling (s/? ::float))
               (fn [{:keys [floor ceiling]
                     :or {floor (float (- Float/MAX_VALUE))
                          ceiling Float/MAX_VALUE}}] (<= floor ceiling)))
  :ret ::float
  :fn (fn [{:keys [args ret]}] (let [{:keys [floor ceiling]
                                      :or   {floor   (- Float/MAX_VALUE)
                                             ceiling Float/MAX_VALUE}} args]
                                 (or (= Float/NaN ret)
                                     (not (Float/isFinite ret))
                                     (and (<= floor ret)
                                          (<= ret ceiling))))))

(s/def ::double (s/and float?
                       #(or (.isNaN %1) (<= %1 Double/MAX_VALUE))
                       #(or (.isNaN %1) (<= (- Double/MAX_VALUE)) %1)
                       (partial not= -0.0)))

(s/fdef undertaker/real-double
  :args (s/and (s/cat :floor (s/? ::double) :ceiling (s/? ::double))
               (fn [{:keys [floor ceiling]
                     :or   {floor   (- Double/MAX_VALUE)
                            ceiling Double/MAX_VALUE}}] (<= floor ceiling)))
  :ret double?
  :fn (fn [{:keys [args ret]}] (let [{:keys [floor ceiling]
                                      :or   {floor   (- Double/MAX_VALUE)
                                             ceiling Double/MAX_VALUE}} args]
                                 (and (<= floor ret)
                                      (<= ret ceiling)))))

(s/fdef undertaker/double
  :args (s/and (s/cat :floor (s/? ::double) :ceiling (s/? ::double))
               (fn [{:keys [floor ceiling]
                     :or   {floor   (- Double/MAX_VALUE)
                            ceiling Double/MAX_VALUE}}] (<= floor ceiling)))
  :ret double?
  :fn (fn [{:keys [args ret]}] (let [{:keys [floor ceiling]
                                      :or   {floor   (- Double/MAX_VALUE)
                                             ceiling Double/MAX_VALUE}} args]
                                 (or (Double/isNaN ret)
                                     (Double/isInfinite ret)
                                     (and (<= floor ret)
                                          (<= ret ceiling))))))

(s/fdef undertaker/string
  :args (s/and (s/cat :min (s/? nat-int?) :max (s/? nat-int?))
               (fn [{:keys [min max]
                     :or   {min 0
                            max (+ min undertaker/default-string-max-size)}}]
                 (<= min max)))
  :ret string?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min 0
                      max (+ min undertaker/default-string-max-size)}} args]
          (and (<= min (count ret))
               (<= (count ret) max)))))

(s/fdef undertaker/string-ascii
  :args (s/and (s/cat :min (s/? nat-int?) :max (s/? nat-int?))
               (fn [{:keys [min max]
                     :or   {min 0
                            max (+ min undertaker/default-string-max-size)}}]
                 (<= min max)))
  :ret string?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min 0
                      max (+ min undertaker/default-string-max-size)}} args]
          (and (<= min (count ret))
               (<= (count ret) max)))))

(s/fdef undertaker/string-alphanumeric
  :args (s/and (s/cat :min (s/? nat-int?) :max (s/? nat-int?))
               (fn [{:keys [min max]
                     :or   {min 0
                            max (+ min undertaker/default-string-max-size)}}]
                 (<= min max)))
  :ret string?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min 0
                      max (+ min undertaker/default-string-max-size)}} args]
          (and (<= min (count ret))
               (<= (count ret) max)))))

(s/fdef undertaker/string-alpha
  :args (s/and (s/cat :min (s/? nat-int?) :max (s/? nat-int?))
               (fn [{:keys [min max]
                     :or   {min 0
                            max (+ min undertaker/default-string-max-size)}}]
                 (<= min max)))
  :ret string?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min max]
               :or   {min 0
                      max (+ min undertaker/default-string-max-size)}} args]
          (and (<= min (count ret))
               (<= (count ret) max)))))

(s/fdef undertaker/elements
  :args (s/cat :coll (s/coll-of any?))
  :ret any?
  :fn (fn [{:keys [args ret]}] (contains? (set (:coll args)) ret)))

(s/fdef undertaker/keyword
  :args (s/cat)
  :ret keyword?)

(s/def ::undertaker/value-gen-takes-key-as-arg boolean?)

(s/fdef undertaker/map-of
  :args (s/cat :key-gen fn?
               :value-gen any?                              ;Can be a function or a etc, it's arity depends on opts.
               :min-size (s/? nat-int?)
               :max-size (s/? nat-int?)
               :opts (s/? (s/keys :opt-un [::value-gen-takes-key-as-arg])))
  :ret map?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [min-size max-size]} args]
          (if (and min-size max-size)
            (<= min-size max-size)
            true))))
