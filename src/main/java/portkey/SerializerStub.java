package portkey;

import clojure.lang.IFn;
import clojure.lang.ILookup;
import clojure.lang.Keyword;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.Serializer;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;

public class SerializerStub<T> extends Serializer<T> implements ILookup {

    public static Keyword READ_KEY = (Keyword) clojure.java.api.Clojure.read(":deser");
    public static Keyword WRITE_KEY = (Keyword) clojure.java.api.Clojure.read(":ser");
    
    private final IFn readfn;
    private final IFn writefn;
    
    public SerializerStub(IFn readfn, IFn writefn) {
        super();
        this.readfn = readfn;
        this.writefn = writefn;
    }

    public void write(Kryo kryo, Output output, T object) {
        writefn.invoke(kryo, output, object);
    }

    @SuppressWarnings("unchecked")
    public T read(Kryo kryo, Input input, Class<T> type) {
        return (T) readfn.invoke(kryo, input, type);
    }

    public Object valAt(Object key) {
        return valAt(key, null);
    }

    public Object valAt(Object key, Object notFound) {
        if (READ_KEY.equals(key)) return readfn;
        if (WRITE_KEY.equals(key)) return writefn;
        return notFound;
    }

}
