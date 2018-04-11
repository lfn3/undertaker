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

(s/fdef shrink/move-towards-0
        :args (s/cat :byte ::bytes/byte)
        :ret ::bytes/byte
        :fn (fn [{:keys [args ret]}]
              (let [{:keys [byte]} args]
                (or (= 0 ret)
                    (< (bit-and 0xff ret)
                       (bit-and 0xff byte))))))

(s/fdef shrink/move-bytes-towards-zero
  :args (s/cat :bytes bytes? :fn fn?)
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

(defn bytes-and-intervals-gen [& other-gens]
  (gen/bind (gen/such-that (complement empty?) gen/bytes)
             (fn [bytes]
               (let [lower (gen/choose 0 (dec (count bytes)))
                     mapped (gen/bind lower
                                      (fn [lower]
                                        (gen/hash-map ::proto/interval-start (gen/return lower)
                                                      ::proto/interval-end (gen/choose lower (dec (count bytes))))))]
                 (apply gen/tuple (gen/return bytes) mapped other-gens)))))

(s/fdef shrink/snip-interval
        :args (s/with-gen (s/and (s/cat :bytes bytes? :interval (s/keys :req [::proto/interval-start
                                                                              ::proto/interval-end]))
                                 interval-inside-bytes?)
                          bytes-and-intervals-gen)
        :ret ::bytes/bytes)

(s/fdef shrink/snip-intervals
  :args (s/with-gen (s/and (s/cat :bytes bytes?
                                  :intervals (s/coll-of (s/keys :req [::proto/interval-start
                                                                      ::proto/interval-end]))
                                  :fn ::undertaker/prop-fn)
                           #(every? (partial interval-inside-bytes? (:bytes %1)) (:intervals %1)))
                    (partial bytes-and-intervals-gen ))
  :ret ::bytes/bytes)

(s/fdef shrink/is-overrun?
        :args (s/cat :result-map (s/keys :req [:net.lfn3.undertaker.core/cause]))
        :ret boolean?)
(s/fdef shrink/is-snippable?
        :args (s/cat :interval (s/keys :opt [::proto/hints]))
        :ret boolean?)
(s/fdef shrink/snip-intervals
        :args (s/cat :bytes ::bytes/bytes :intervals ::proto/completed-intervals :fn fn?)
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
                     :fn fn?)
        :ret ::undertaker/results-map)
