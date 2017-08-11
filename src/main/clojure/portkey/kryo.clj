(ns portkey.kryo
  (:require
    [clojure.java.io :as io]
    [portkey.logdep :refer [log-dep]]
    [carbonite.api :as carb]
    [carbonite.serializer :as ser]))

(defn serializer [read write]
  (portkey.SerializerStub. read write))

(def default-serializers 
  {clojure.lang.Var
   (serializer
     (fn [_ k ^com.esotericsoftware.kryo.io.Input input type]
       (let [sym (symbol (.readString input))]
         (clojure.java.api.Clojure/var sym))) ; intern the var (creating its ns if necessary)
     (fn [_ k ^com.esotericsoftware.kryo.io.Output output ^clojure.lang.Var v]
       (log-dep :var v)
       (if-some [ns-name (some-> v .ns .name)]
         (.writeString output (str ns-name "/" (.sym v)))
         (throw (ex-info "TODO non interned vars" {:var v})))))
   clojure.lang.Namespace
   (serializer
     (fn [_ k ^com.esotericsoftware.kryo.io.Input input type]
       (let [ns-name (symbol (.readString input))]
         (clojure.lang.Namespace/findOrCreate ns-name)))
     (fn [_ k ^com.esotericsoftware.kryo.io.Output output ^clojure.lang.Namespace ns]
       (.writeString output (name (.name ns)))))
   clojure.lang.ITransientCollection
   (serializer
     (fn read-transient [_ ^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Input input class]
       (transient (.readClassAndObject kryo input)))
     (fn write-transient [_ ^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Output output coll]
       (.writeClassAndObject kryo output (persistent! coll))))
   clojure.lang.IPersistentMap
   (serializer
     (fn [_ ^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Input input class]
       (loop [remaining (.readInt input true)
              m (.newInstance kryo class)]
         (if (zero? remaining)
           m
           (recur (dec remaining)
             (assoc m
               (.readClassAndObject kryo input)
               (.readClassAndObject kryo input))))))
     (fn [_ ^com.esotericsoftware.kryo.Kryo kryo ^com.esotericsoftware.kryo.io.Output output coll]
       (ser/write-map kryo output coll)))
   clojure.lang.LazySeq
   com.esotericsoftware.kryo.serializers.FieldSerializer
   clojure.lang.Iterate
   com.esotericsoftware.kryo.serializers.FieldSerializer
   clojure.lang.LongRange
   com.esotericsoftware.kryo.serializers.FieldSerializer
   clojure.lang.Range
   com.esotericsoftware.kryo.serializers.FieldSerializer})

(defn register-default-serializers [^com.esotericsoftware.kryo.Kryo kryo m]
  (doseq [[^Class class serializer] m]
    (.addDefaultSerializer kryo class serializer)))

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
      carb/default-registry
      (.setInstantiatorStrategy (org.objenesis.strategy.StdInstantiatorStrategy.)) ; for closures
      (.setClassLoader (clojure.lang.RT/baseLoader))
      (register-default-serializers default-serializers)
      (.setDefaultSerializer wrapped-factory))))

(defn freeze [x]
  (let [classes (atom #{})
        vars (atom #{})
        kryo (mk-kryo)
        baos (java.io.ByteArrayOutputStream.)]
    (with-open [out (com.esotericsoftware.kryo.io.Output. baos)]
      (.writeClassAndObject kryo out x))
    (.toByteArray baos)))

(defn unfreeze [in]
  (with-open [in (io/input-stream in)]
    (let [kryo (mk-kryo)]
      (->> in com.esotericsoftware.kryo.io.Input.
        (.readClassAndObject kryo)))))
