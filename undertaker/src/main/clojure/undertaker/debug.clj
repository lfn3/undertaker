(ns undertaker.debug)

(def ^:const debug-mode (Boolean/valueOf (System/getProperty "undertaker.debug")))

(when debug-mode
  (prn "Undertaker is currently running in debug mode!!!")
  (prn "This will make your tests a lot slower."))
