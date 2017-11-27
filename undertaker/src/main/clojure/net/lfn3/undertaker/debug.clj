(ns net.lfn3.undertaker.debug
  (:import (net.lfn3.undertaker UndertakerDebugException)))

(def ^:const debug-mode (Boolean/valueOf (System/getProperty "net.lfn3.undertaker.undertaker.debug")))

(when debug-mode
  (println "Undertaker is currently running in debug mode!!!")
  (println "This will make your tests a lot slower."))

(defn internal-exception
  ([msg info] (internal-exception msg info nil))
  ([msg info cause]
   (UndertakerDebugException. (ex-info msg info cause))))
