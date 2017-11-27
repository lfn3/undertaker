(ns net.lfn3.undertaker.specs.shrink
  (:require [clojure.spec.alpha :as s]
            [net.lfn3.undertaker.shrink :as shrink]
            [net.lfn3.undertaker.proto :as proto]
            [net.lfn3.undertaker.bytes :as bytes]
            [net.lfn3.undertaker.source :as source]
            [clojure.test.check.generators :as gen]))

(s/fdef shrink/move-towards-0
        :args (s/cat :byte ::bytes/byte)
        :ret ::bytes/byte
        :fn (fn [{:keys [args ret]}]
              (let [{:keys [byte]} args]
                (or (= 0 ret)
                    (< (bit-and 0xff ret)
                       (bit-and 0xff (:byte args)))))))
(s/fdef shrink/snip-interval
        :args (s/cat :bytes ::bytes/bytes :interval (s/keys :req [::proto/interval-start ::proto/interval-end]))
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
        :ret ::source/source)
