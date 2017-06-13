package portkey.analysis;

import org.objectweb.asm.Type;
import org.objectweb.asm.tree.analysis.Value;

import clojure.lang.ILookup;
import clojure.lang.Keyword;

public class UCValue implements Value, ILookup {

    public static final Value REFERENCE_VALUE = new UCValue(Type.getObjectType("java/lang/Object"));;
    public static final Value INT_VALUE = new UCValue(Type.INT_TYPE);
    public static final Value DOUBLE_VALUE = new UCValue(Type.DOUBLE_TYPE);
    public static final Value LONG_VALUE = new UCValue(Type.LONG_TYPE);
    public static final Value FLOAT_VALUE = new UCValue(Type.FLOAT_TYPE);
    public static final Value RETURNADDRESS_VALUE = new UCValue(Type.VOID_TYPE);
    public static final Value UNINITIALIZED_VALUE = new UCValue(null);
    
    public final Type type;
    public final Object value;
    public final boolean constant;
    public final boolean unknown;
    
    public static final Keyword TYPE_KW = Keyword.intern("type");
    public static final Keyword VALUE_KW = Keyword.intern("value");
    public static final Keyword CONSTANT_KW = Keyword.intern("constant?");
    public static final Keyword UNKNOWN_KW = Keyword.intern("unknown?");

    public Object valAt(Object key) {
        return valAt(key, null);
    }

    public Object valAt(Object key, Object notFound) {
        if (TYPE_KW.equals(key)) return type;
        if (VALUE_KW.equals(key)) return value;
        if (CONSTANT_KW.equals(key)) return constant;
        if (UNKNOWN_KW.equals(key)) return unknown;        
        return notFound;
    }    

    public UCValue(Type type) {
        this.type = type;
        this.value = null;
        this.constant = false;
        this.unknown = false;
    }
    
    public UCValue(Type type, Object value) {
        this.type = type;
        this.value = value;
        this.constant = true;
        this.unknown = false;
    }
    
    public UCValue(Type type, Object value, boolean constant, boolean unknown) {
        this.type = type;
        this.value = value;
        this.constant = constant;
        this.unknown = unknown;
    }
    
    public int getSize() {
        return type == Type.LONG_TYPE || type == Type.DOUBLE_TYPE ? 2 : 1;
    }

}
