(ns portkey.analysis
  (:require [portkey.logdep :refer [log-dep]]))

(def CLASS (portkey.analysis.UCValue. (org.objectweb.asm.Type/getObjectType "java/lang/Class")))
(def VAR (portkey.analysis.UCValue. (org.objectweb.asm.Type/getObjectType "clojure/lang/Var")))
(def UNKNOWN_CLASS (portkey.analysis.UCValue. (org.objectweb.asm.Type/getObjectType "java/lang/Class")
                     nil false true))

(def dispatch
 {[true "java/lang/Class" "forName" "(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;"]
  (fn [classname] (if (:constant? classname) CLASS UNKNOWN_CLASS))
  [true "java/lang/Class" "forName" "(Ljava/lang/String;)Ljava/lang/Class;"]
  (fn [classname _ _] (if (:constant? classname) CLASS UNKNOWN_CLASS))
  [true "clojure/lang/RT" "var" "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;"]
  (fn [ns name]
    (if (and (:constant? ns) (:constant? name))
      (do (log-dep :var-ref (symbol (:value ns) (:value name))) VAR) ; could even be made constant
      (throw (ex-info "Can't statically resolve var lookup" {:ns ns :name name}))))})

(defn invoke [is-static owner name desc args]
;  (prn is-static owner name desc)
  (if-some [f (dispatch [is-static owner name desc])]
    (apply f args)
    (portkey.analysis.UCValue. (.getReturnType (org.objectweb.asm.Type/getMethodType desc)))))
