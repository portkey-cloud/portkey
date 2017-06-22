(ns portkey.analysis
  (:require [portkey.logdep :refer [log-dep]]))

(def CLASS (portkey.analysis.UCValue. (org.objectweb.asm.Type/getObjectType "java/lang/Class")))
(def OBJECT (portkey.analysis.UCValue. (org.objectweb.asm.Type/getObjectType "java/lang/Object")))
(def VAR (portkey.analysis.UCValue. (org.objectweb.asm.Type/getObjectType "clojure/lang/Var")))
(def UNKNOWN_CLASS (portkey.analysis.UCValue. (org.objectweb.asm.Type/getObjectType "java/lang/Class")
                     nil false true))
(def UNKNOWN_OBJECT (portkey.analysis.UCValue. (org.objectweb.asm.Type/getObjectType "java/lang/Object")
                      nil false true))

(defn- asbtract-load-class [classname]
  (if (:constant? classname)
    (do
      (log-dep :class (:value classname))
      CLASS)
    UNKNOWN_CLASS))

(def dispatch
 {[true "java/lang/Class" "forName" "(Ljava/lang/String;)Ljava/lang/Class;"]
  asbtract-load-class
  [true "java/lang/Class" "forName" "(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;"]
  (fn [classname _ _] (if (:constant? classname) 
                        (do
                          (log-dep :class (:value classname))
                          CLASS)
                        UNKNOWN_CLASS))
  [false "java/lang/Class" "newInstance" "()Ljava/lang/Object;"]
  (fn [class]
    (if (:unknown? class)
      UNKNOWN_OBJECT
      OBJECT))
  [false "java/lang/Class" "getConstructor" "([Ljava/lang/Class;)Ljava/lang/reflect/Constructor;"]
  (fn [class _]
    (if (:unknown? class)
      UNKNOWN_OBJECT
      OBJECT))
  [false "java/lang/reflect/Constructor" "newInstance" "([Ljava/lang/Object;)Ljava/lang/Object;"]
  (fn [ctor _]
    (if (:unknown? ctor)
      UNKNOWN_OBJECT
      OBJECT))
  [true "clojure/lang/RT" "classForName" "(Ljava/lang/String;)Ljava/lang/Class;"]
  asbtract-load-class
  [true "clojure/lang/RT" "var" "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;"]
  (fn [ns name]
    (if (and (:constant? ns) (:constant? name))
      (do (log-dep :var-ref (symbol (:value ns) (:value name))) VAR) ; could even be made constant
      (throw (ex-info "Can't statically resolve var lookup" {:ns ns :name name}))))
  [true "clojure/lang/Util" "loadWithClass" "(Ljava/lang/String;Ljava/lang/Class;)Ljava/lang/Object;"]
  (fn [script-base _]
    (log-dep :class (str script-base clojure.lang.RT/LOADER_SUFFIX))
    OBJECT)
  ;; the amount of adhoc interpretations should be minimized
  [true "org/apache/hadoop/util/ReflectionUtils" "newInstance"
   "(Ljava/lang/Class;Lorg/apache/hadoop/conf/Configuration;)Ljava/lang/Object;"]
  (fn [class conf]
    (if (:unknown? class)
      UNKNOWN_OBJECT
      OBJECT))
  [false "org/apache/hadoop/conf/Configuration" "getClass"
   "(Ljava/lang/String;Ljava/lang/Class;Ljava/lang/Class;)Ljava/lang/Class;"]
  (fn [conf classname parent iface]
    (do
      (log-dep :class (:value classname))
      CLASS)
    UNKNOWN_CLASS)})

(defn invoke [is-static owner name desc args]
  #_(prn is-static owner name desc)
  (if-some [f (dispatch [is-static owner name desc])]
    (apply f args)
    (portkey.analysis.UCValue. (.getReturnType (org.objectweb.asm.Type/getMethodType desc)))))
