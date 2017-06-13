(ns portkey.logdep)

;; this package is packaged with generated lambdas (because the kryo ns has a dep on log-dep)
;; so it should be kept to the bare minimum

(def ^:dynamic log-dep (constantly nil))

