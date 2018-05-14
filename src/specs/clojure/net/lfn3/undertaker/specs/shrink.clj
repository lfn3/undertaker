(ns net.lfn3.undertaker.specs.shrink
  (:require [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.shrink :as shrink]
            [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.specs.bytes]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.source :as source]
            [net.lfn3.undertaker.specs.core]
            [net.lfn3.undertaker.core :as undertaker]
            [clojure.test.check.generators :as gen]))

(s/def ::interval-only-start-and-end (s/keys :req [::proto/interval-start
                                                   ::proto/interval-end]))

(s/fdef shrink/move-towards-0
        :args (s/cat :byte ::bytes/byte)
        :ret ::bytes/byte
        :fn (fn [{:keys [args ret]}]
              (let [{:keys [byte]} args]
                (or (= 0 ret)
                    (< (bit-and 0xff ret)
                       (bit-and 0xff byte))))))

(s/fdef shrink/move-bytes-towards-zero
  :args (s/cat :bytes bytes? :fn ::undertaker/wrapped-prop-fn)
  :ret bytes?
  :fn (fn [{:keys [args ret]}]
        (let [{:keys [bytes]} args]
          (= (count bytes) (count ret)))))

(defn interval-inside-bytes?
  ([{:keys [bytes interval]}] (interval-inside-bytes? bytes interval))
  ([bytes interval]
   (let [{:keys [::proto/interval-start ::proto/interval-end]} interval
         arr-size (count bytes)
         passed? (and (<= interval-start interval-end)
                      (< interval-start arr-size)
                      (<= interval-end arr-size))]
     (when-not passed?
       (println "Interval not inside bytes:" (vec bytes)))
     passed?)))

(defn interval-inside-bytes-gen [bytes]
  (let [lower (gen/choose 0 (dec (count bytes)))]
    (gen/bind lower
              (fn [lower]
                (gen/hash-map ::proto/interval-start (gen/return lower)
                              ::proto/interval-end (gen/choose lower (dec (count bytes))))))))

(defn bytes-and-interval-gen [& other-gens]
  (gen/bind (gen/such-that (complement empty?) gen/bytes)
            #(apply gen/tuple (gen/return %1) (interval-inside-bytes-gen %1) other-gens)))

(s/fdef shrink/snip-interval
        :args (s/with-gen (s/and (s/cat :bytes bytes? :interval ::interval-only-start-and-end)
                                 interval-inside-bytes?)
                          bytes-and-interval-gen)
        :ret ::bytes/bytes)

(s/fdef shrink/is-overrun?
        :args (s/cat :result-map (s/keys :req [:net.lfn3.undertaker.core/cause]))
        :ret boolean?)
(s/fdef shrink/is-snippable?
        :args (s/cat :interval (s/keys :opt [::proto/hints]))
        :ret boolean?)

#_(s/fdef shrink/snip-intervals
  :args (s/with-gen
          (s/and (s/cat :bytes ::bytes/bytes
                        :intervals (s/coll-of (s/keys :req [::proto/interval-start
                                                            ::proto/interval-end]))
                        :fn ::undertaker/prop-fn)
                 (every? (partial interval-inside-bytes? (:bytes %1)) (:intervals %1)))
          (partial bytes-and-interval-gen (s/gen ::undertaker/wrapped-prop-fn)))
  :ret ::bytes/bytes)

(defn bytes-and-intervals-gen [& other-gens]
  (gen/bind (gen/such-that (complement empty?) gen/bytes)
            #(apply gen/tuple (gen/return %1) (gen/vector (interval-inside-bytes-gen %1)) other-gens)))

(s/fdef shrink/snip-intervals
        :args (s/with-gen
                (s/cat :bytes ::bytes/bytes
                       :intervals (s/coll-of ::interval-only-start-and-end)
                       :fn ::undertaker/wrapped-prop-fn)
                #(bytes-and-intervals-gen (s/gen ::undertaker/wrapped-prop-fn)))
        :ret ::bytes/bytes)

(s/fdef shrink/shrink-at!
        :args (s/with-gen (s/cat :bytes (s/and bytes?
                                               not-empty)
                                 :index integer?)
                          #(gen/bind (s/gen (s/and bytes?
                                                   not-empty))
                                     (fn [byte-arr]
                                       (gen/tuple (gen/return byte-arr)
                                                  (gen/choose 0 (dec (count byte-arr)))))))
        :ret bytes?
        :fn (fn [{:keys [args ret]}]
              (let [{:keys [bytes index]} args]
                (and (< index (count bytes))
                     (= (count bytes)
                        (count ret))
                     (>= (shrink/sum-abs bytes)
                         (shrink/sum-abs ret))))))
(s/fdef shrink/shrink
        :args (s/cat :bytes ::source/source
                     :fn ::undertaker/wrapped-prop-fn)
        :ret ::undertaker/results-map)
