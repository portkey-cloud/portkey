(ns portkey.logdep)

(defn- canon [^Class c]
  (.replace (.getCanonicalName c) \. \/))

(def ^:dynamic *log-dep* (constantly nil))

(defn log-dep [type x]
  (*log-dep*
    (case type
     :var x
     :class (canon x)
     :var-ref (resolve x) ; TODO warn on nil
     :class-name x)))
