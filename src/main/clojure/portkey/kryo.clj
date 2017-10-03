(ns portkey.kryo
  (:require
    [clojure.java.io :as io]
    [portkey.logdep :refer [log-dep]]))

(defn serializer [& {:keys [ser deser]}]
  (portkey.SerializerStub. deser ser))

(def pr-serializer
  (serializer
    :ser (fn [k ^com.esotericsoftware.kryo.io.Output output x]
           (.writeString output (pr-str x)))
    :deser (fn [k ^com.esotericsoftware.kryo.io.Input input type]
             (read-string (.readString input)))))

(defn wrap-serializer [& {wrap-ser :ser wrap-deser :deser}]
  (fn [{:keys [ser deser]}]
    (serializer :ser (wrap-ser ser) :deser (wrap-deser deser))))

(def wrap-meta
  (wrap-serializer
    :ser (fn [ser]
           (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Output output x]
             (let [m (meta x)]
               (.writeInt output (count m) true)
               (run!
                 (fn [[k v]]
                   (.writeClassAndObject kryo output k)
                   (.writeClassAndObject kryo output v))
                 m)
               (ser kryo output x))))
    :deser (fn [deser]
             (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Input input type]
               (let [n (.readInt input true)
                     m (into {} (map (fn [_] [(.readClassAndObject kryo input) (.readClassAndObject kryo input)])) (range n))]
                 (cond-> (deser kryo input type)
                   (pos? n) (with-meta m)))))))

(defn coll-serializer [ser-empty deser-empty]
  (serializer
     :deser (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Input input class]
              (let [x (deser-empty kryo input class)
                    remaining (.readInt input true)]
                (into x (map (fn [_] (.readClassAndObject kryo input))) (range remaining))))
     :ser (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Output output coll]
            (ser-empty kryo output coll)
            (.writeInt output (count coll) true)
            (run!
              (fn [v]
                (.writeClassAndObject kryo output v))
              coll))))

(defn map-serializer [ser-empty deser-empty]
  (serializer
     :deser (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Input input class]
              (let [m (deser-empty kryo input class)
                    remaining (.readInt input true)]
                (into m (map (fn [_] [(.readClassAndObject kryo input) (.readClassAndObject kryo input)])) (range remaining))))
     :ser (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Output output coll]
            (ser-empty kryo output coll)
            (.writeInt output (count coll) true)
            (run!
             (fn [[k v]]
               (.writeClassAndObject kryo output k)
               (.writeClassAndObject kryo output v))
             coll))))

#_(def serializer-factory
   (reify com.esotericsoftware.kryo.factories.SerializerFactory
     (makeSerializer [factory kryo class] (make-serializer class))))

(def serializers 
  (array-map ; for stable order
    clojure.lang.Keyword pr-serializer
    clojure.lang.Symbol  pr-serializer
    clojure.lang.Ratio   pr-serializer
    clojure.lang.BigInt pr-serializer
    clojure.lang.Var
    (serializer
      :deser (fn [k ^com.esotericsoftware.kryo.io.Input input type]
               (let [sym (symbol (.readString input))]
                 (clojure.java.api.Clojure/var sym))) ; intern the var (creating its ns if necessary)
      :ser (fn [k ^com.esotericsoftware.kryo.io.Output output ^clojure.lang.Var v]
             (log-dep :var v)
             (if-some [ns-name (some-> v .ns .name)]
               (.writeString output (str ns-name "/" (.sym v)))
               (throw (ex-info "TODO non interned vars" {:var v})))))
    clojure.lang.Namespace
    (serializer
      :deser (fn [k ^com.esotericsoftware.kryo.io.Input input type]
               (let [ns-name (symbol (.readString input))]
                 (clojure.lang.Namespace/findOrCreate ns-name)))
      :ser (fn [k ^com.esotericsoftware.kryo.io.Output output ^clojure.lang.Namespace ns]
             (.writeString output (name (.name ns)))))
    clojure.lang.PersistentVector (wrap-meta (coll-serializer (constantly nil) (constantly [])))
    clojure.lang.PersistentHashSet (wrap-meta (coll-serializer (constantly nil) (constantly #{})))
    clojure.lang.PersistentTreeSet (wrap-meta (coll-serializer (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Output output ^clojure.lang.Sorted coll]
                                                                 (.writeClassAndObject kryo output (.comparator coll)))
                                                (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Input input _]
                                                  (sorted-set-by (.readClassAndObject kryo input)))))
    clojure.lang.PersistentArrayMap (wrap-meta (coll-serializer (constantly nil) (constantly {})))
    clojure.lang.PersistentHashMap (wrap-meta (coll-serializer (constantly nil) (constantly clojure.lang.PersistentHashMap/EMPTY)))
    clojure.lang.PersistentTreeMap (wrap-meta (coll-serializer (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Output output ^clojure.lang.Sorted coll]
                                                                 (.writeClassAndObject kryo output (.comparator coll)))
                                                (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Input input _]
                                                  (sorted-map-by (.readClassAndObject kryo input)))))
    clojure.lang.MapEntry (serializer
                            :ser (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Output output kv]
                                   (.writeClassAndObject kryo output (key kv))
                                   (.writeClassAndObject kryo output (val kv)))
                            :deser (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Input input _]
                                     (clojure.lang.MapEntry. (.readClassAndObject kryo input)
                                       (.readClassAndObject kryo input))))))

(def default-serializers
  (array-map
    clojure.lang.IPersistentCollection (coll-serializer (constantly nil) (fn [^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Input input type]
                                                                           (.newInstance kryo type)))))

(defn register-default-serializers [^com.esotericsoftware.kryo.Kryo kryo m]
  (doseq [[^Class class serializer] m]
    (.addDefaultSerializer kryo class serializer)))

(defn register-serializers [^com.esotericsoftware.kryo.Kryo kryo m]
  (doseq [[^Class class ^com.esotericsoftware.kryo.Serializer serializer] m]
    (.register kryo class serializer)))

(defn- ^com.esotericsoftware.kryo.Kryo mk-kryo []
  (let [default-factory (com.esotericsoftware.kryo.factories.ReflectionSerializerFactory. com.esotericsoftware.kryo.serializers.FieldSerializer)
        wrapped-factory
        (reify com.esotericsoftware.kryo.factories.SerializerFactory
          (makeSerializer [_ k class]
            (log-dep :class class)
            (let [ser (.makeSerializer default-factory k class)]
              (when (instance? com.esotericsoftware.kryo.serializers.FieldSerializer ser)
                (.setSerializeTransient ^com.esotericsoftware.kryo.serializers.FieldSerializer ser true))
              ser)))]
    (doto (com.esotericsoftware.kryo.Kryo.)
      (register-default-serializers default-serializers)
      (register-serializers serializers)
      (.setInstantiatorStrategy (org.objenesis.strategy.StdInstantiatorStrategy.)) ; for closures
      (.setClassLoader (clojure.lang.RT/baseLoader))
      (.setDefaultSerializer wrapped-factory))))

(defn freeze [x]
  (let [classes (atom #{})
        vars (atom #{})
        kryo (mk-kryo)
        baos (java.io.ByteArrayOutputStream.)]
    (binding [*print-readably* true
              *print-meta* true
              *print-level* nil
              *print-length* nil
              *print-namespace-maps* true
              *print-dup* false]
      (with-open [out (com.esotericsoftware.kryo.io.Output. baos)]
       (.writeClassAndObject kryo out x)))
    (.toByteArray baos)))

(defn unfreeze [in]
  (with-open [in (io/input-stream in)]
    (let [kryo (mk-kryo)]
      (->> in com.esotericsoftware.kryo.io.Input.
        (.readClassAndObject kryo)))))
