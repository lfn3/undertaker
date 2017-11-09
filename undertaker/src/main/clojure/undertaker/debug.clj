(ns undertaker.debug
  (:import (com.lmax.undertaker UndertakerDebugException)))

(def ^:const debug-mode (Boolean/valueOf (System/getProperty "undertaker.debug")))

(when debug-mode
  (println "Undertaker is currently running in debug mode!!!")
  (println "This will make your tests a lot slower."))

(defn internal-exception
  ([msg info] (internal-exception msg info nil))
  ([msg info cause]
   (UndertakerDebugException. (ex-info msg info cause))))
