(ns portkey.kryo
  (:require
    [clojure.java.io :as io]
    [portkey.logdep :refer [log-dep]]))

(defn- ^com.esotericsoftware.kryo.Kryo mk-kryo []
  (let [default-factory (com.esotericsoftware.kryo.factories.ReflectionSerializerFactory. com.esotericsoftware.kryo.serializers.FieldSerializer)
        wrapped-factory
        (reify com.esotericsoftware.kryo.factories.SerializerFactory
          (makeSerializer [_ k class]
            (log-dep :class class)
            (.makeSerializer default-factory k class)))]
    (doto (com.esotericsoftware.kryo.Kryo.)
      (.setInstantiatorStrategy (org.objenesis.strategy.StdInstantiatorStrategy.)) ; for closures
      (.setClassLoader (clojure.lang.RT/baseLoader))
      (.addDefaultSerializer clojure.lang.Var
        (portkey.SerializerStub.
          (fn [_ k ^com.esotericsoftware.kryo.io.Input input type]
            (let [sym (symbol (.readString input))]
              (clojure.java.api.Clojure/var sym))) ; intern the var (creating its ns if necessary)
          (fn [_ k ^com.esotericsoftware.kryo.io.Output output ^clojure.lang.Var v]
            (log-dep :var v)
            (if-some [ns-name (some-> v .ns .name)]
              (.writeString output (str ns-name "/" (.sym v)))
              (throw (ex-info "TODO non interned vars" {:var v}))))))
      ; TODO add serializers for immutable collections
      (.addDefaultSerializer clojure.lang.LazySeq
        com.esotericsoftware.kryo.serializers.FieldSerializer)
      (.addDefaultSerializer clojure.lang.Iterate
        com.esotericsoftware.kryo.serializers.FieldSerializer)
      (.addDefaultSerializer clojure.lang.LongRange
        com.esotericsoftware.kryo.serializers.FieldSerializer)
      (.addDefaultSerializer clojure.lang.Range
        com.esotericsoftware.kryo.serializers.FieldSerializer)
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
