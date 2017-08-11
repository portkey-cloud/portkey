(ns portkey.analysis
  (:require [portkey.logdep :refer [log-dep]]))


(defrecord Value [^org.objectweb.asm.Type type constant? value]
  org.objectweb.asm.tree.analysis.Value
  (getSize [_] (if type (.getSize type) 1)))

(defn new-value
  ([type]
    (Value. type false nil))
  ([type value]
    (Value. type true value)))

(def UNDEFINED (new-value nil))
(def A_CLASS (new-value (org.objectweb.asm.Type/getObjectType "java/lang/Class")))
(def A_VAR (new-value (org.objectweb.asm.Type/getObjectType "clojure/lang/Var")))
(def AN_OBJECT (new-value (org.objectweb.asm.Type/getObjectType "java/lang/Object")))

(defn reftype? [^org.objectweb.asm.Type t]
  (let [sort (some-> t .getSort)]
    (or (= sort org.objectweb.asm.Type/ARRAY) (= sort org.objectweb.asm.Type/OBJECT))))

(defn merge-vals [a b]
  (cond
    (= a b) a
    ; same type but different -> different values
    (= (:type a) (:type b)) (new-value (:type a))
    (and (reftype? (:type a)) (reftype? (:type b))) AN_OBJECT
    :else UNDEFINED))

(defn- abstract-load-class [classname]
  (if (:constant? classname)
    (let [classname (.replace ^String (:value classname) \. \/)]
      (log-dep :class classname)
      (new-value (org.objectweb.asm.Type/getObjectType "java/lang/Class")
        (org.objectweb.asm.Type/getObjectType classname)))
    A_CLASS))

(def dispatch
 {[true "java/lang/Class" "forName" "(Ljava/lang/String;)Ljava/lang/Class;"]
  abstract-load-class
  [true "java/lang/Class" "forName" "(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;"]
  (fn [classname _ _] (abstract-load-class classname))
  [false "java/lang/Class" "newInstance" "()Ljava/lang/Object;"]
  (fn [class]
    (if (:constant? class)
      (new-value (:value class))
      UNDEFINED))
  [false "java/lang/Class" "getConstructor" "([Ljava/lang/Class;)Ljava/lang/reflect/Constructor;"]
  (fn [class _]
    (cond->
      (new-value (org.objectweb.asm.Type/getObjectType "java/lang/reflect/Constructor"))
      (:constant? class) (assoc :ctor-of (:value class))))
  [false "java/lang/reflect/Constructor" "newInstance" "([Ljava/lang/Object;)Ljava/lang/Object;"]
  (fn [ctor _]
    (if-some [class (:ctor-of ctor)]
      (new-value class)
      UNDEFINED))
  [true "clojure/lang/RT" "classForName" "(Ljava/lang/String;)Ljava/lang/Class;"]
  abstract-load-class
  [true "clojure/lang/RT" "var" "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;"]
  (fn [ns name]
    (if (and (:constant? ns) (:constant? name))
      (do (log-dep :var-ref (symbol (:value ns) (:value name))) A_VAR) ; could even be made constant
      (throw (ex-info "Can't statically resolve var lookup" {:ns ns :name name}))))
  [true "clojure/lang/Util" "loadWithClass" "(Ljava/lang/String;Ljava/lang/Class;)Ljava/lang/Object;"]
  (fn [script-base _]
    (when (:constant? script-base)
      #_(log-dep :fake (subs (str (:value script-base) clojure.lang.RT/LOADER_SUFFIX) 1))
      (log-dep :fake (subs (:value script-base) 1)))
    UNDEFINED)
  [true "clojure/lang/Var" "internPrivate" "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;"]
  (fn [ns name]
    ; is it used by anything but genclass?
    (if (and (:constant? ns) (:constant? name))
      (do (log-dep :var-ref (symbol (:value ns) (:value name))) A_VAR) ; could even be made constant
      (throw (ex-info "Can't statically resolve var lookup" {:ns ns :name name}))))
  ;; the amount of adhoc interpretations should be minimized
  [true "org/apache/hadoop/util/ReflectionUtils" "newInstance"
   "(Ljava/lang/Class;Lorg/apache/hadoop/conf/Configuration;)Ljava/lang/Object;"]
  (fn [class conf]
    (if (:constant class)
      (new-value (:value class))
      UNDEFINED))
  [false "org/apache/hadoop/conf/Configuration" "getClass"
   "(Ljava/lang/String;Ljava/lang/Class;Ljava/lang/Class;)Ljava/lang/Class;"]
  (fn [conf classname parent iface]
    (abstract-load-class classname))})

(defn invoke [is-static owner name desc args]
  #_(prn is-static owner name desc)
  (if-some [f (dispatch [is-static owner name desc])]
    (apply f args)
    (new-value (.getReturnType (org.objectweb.asm.Type/getMethodType desc)))))
