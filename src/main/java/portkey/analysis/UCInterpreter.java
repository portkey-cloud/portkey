package portkey.analysis;

import java.util.List;

import org.objectweb.asm.Handle;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.FieldInsnNode;
import org.objectweb.asm.tree.IntInsnNode;
import org.objectweb.asm.tree.InvokeDynamicInsnNode;
import org.objectweb.asm.tree.LdcInsnNode;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.MultiANewArrayInsnNode;
import org.objectweb.asm.tree.TypeInsnNode;
import org.objectweb.asm.tree.analysis.AnalyzerException;
import org.objectweb.asm.tree.analysis.Value;
import org.objectweb.asm.tree.analysis.Interpreter;

import clojure.lang.IFn;
import clojure.lang.Keyword;
import static org.objectweb.asm.Opcodes.*;
import static clojure.java.api.Clojure.*;

public class UCInterpreter extends Interpreter {
    public static final IFn invoke;
    public static final IFn logdep;
    public static final IFn merge;
    public static final IFn newValue;
    public static final Keyword TYPE_KW = Keyword.intern("type");
    public static final Keyword CLASS_KW = Keyword.intern("class");
    public static final Keyword ROOTCLASS_KW = Keyword.intern("root-class");
    
    static {
        var("clojure.core/require").invoke(read("portkey.analysis"), read("portkey.logdep"));
        invoke = var("portkey.analysis/invoke");
        logdep = var("portkey.logdep/log-dep");
        merge = var("portkey.analysis/merge-vals");
        newValue = var("portkey.analysis/new-value");
    }

    public static final Value REFERENCE_VALUE = (Value) newValue.invoke(Type.getObjectType("java/lang/Object"));;
    public static final Value INT_VALUE = (Value) newValue.invoke(Type.INT_TYPE);
    public static final Value DOUBLE_VALUE = (Value) newValue.invoke(Type.DOUBLE_TYPE);
    public static final Value LONG_VALUE = (Value) newValue.invoke(Type.LONG_TYPE);
    public static final Value FLOAT_VALUE = (Value) newValue.invoke(Type.FLOAT_TYPE);
    public static final Value RETURNADDRESS_VALUE = (Value) newValue.invoke(Type.VOID_TYPE);
    public static final Value UNINITIALIZED_VALUE = (Value) newValue.invoke(null);
    
    static void logdep(Type t) {
        logdep.invoke(CLASS_KW, t.getClassName());
    }
    
    static void logdep(String s) {
        logdep.invoke(CLASS_KW, s);
    }
    
    public UCInterpreter() {
        super(ASM5);
    }

    @Override
    public Value newValue(final Type type) {
        if (type == null) {
            return UNINITIALIZED_VALUE;
        }
        switch (type.getSort()) {
        case Type.VOID:
            return null;
        case Type.BOOLEAN:
        case Type.CHAR:
        case Type.BYTE:
        case Type.SHORT:
        case Type.INT:
            return INT_VALUE;
        case Type.FLOAT:
            return FLOAT_VALUE;
        case Type.LONG:
            return LONG_VALUE;
        case Type.DOUBLE:
            return DOUBLE_VALUE;
        case Type.ARRAY:
        case Type.OBJECT:
            return REFERENCE_VALUE;
        default:
            throw new Error("Internal error");
        }
    }

    @Override
    public Value newOperation(final AbstractInsnNode insn)
            throws AnalyzerException {
        switch (insn.getOpcode()) {
        case ACONST_NULL:
            return newValue(Type.getObjectType("null"));
        case ICONST_M1: return (Value) newValue.invoke(Type.INT_TYPE, -1);
        case ICONST_0: return (Value) newValue.invoke(Type.INT_TYPE, 0);
        case ICONST_1: return (Value) newValue.invoke(Type.INT_TYPE, 1);
        case ICONST_2: return (Value) newValue.invoke(Type.INT_TYPE, 2);
        case ICONST_3: return (Value) newValue.invoke(Type.INT_TYPE, 3);
        case ICONST_4: return (Value) newValue.invoke(Type.INT_TYPE, 4);
        case ICONST_5: return (Value) newValue.invoke(Type.INT_TYPE, 5);
        case LCONST_0: return (Value) newValue.invoke(Type.LONG_TYPE, 0);
        case LCONST_1: return (Value) newValue.invoke(Type.LONG_TYPE, 1);
        case FCONST_0: return (Value) newValue.invoke(Type.FLOAT_TYPE, 0);
        case FCONST_1: return (Value) newValue.invoke(Type.FLOAT_TYPE, 1);
        case FCONST_2: return (Value) newValue.invoke(Type.FLOAT_TYPE, 2);
        case DCONST_0: return (Value) newValue.invoke(Type.DOUBLE_TYPE, 0);
        case DCONST_1: return (Value) newValue.invoke(Type.DOUBLE_TYPE, 1);
        case BIPUSH: 
        case SIPUSH:
            return (Value) newValue.invoke(Type.INT_TYPE, ((IntInsnNode) insn).operand);
        case LDC:
            Object cst = ((LdcInsnNode) insn).cst;
            if (cst instanceof Integer) {
                return (Value) newValue.invoke(Type.INT_TYPE, cst);
            } else if (cst instanceof Float) {
                return (Value) newValue.invoke(Type.FLOAT_TYPE, cst);
            } else if (cst instanceof Long) {
                return (Value) newValue.invoke(Type.LONG_TYPE, cst);
            } else if (cst instanceof Double) {
                return (Value) newValue.invoke(Type.DOUBLE_TYPE, cst);
            } else if (cst instanceof String) {
                return (Value) newValue.invoke(Type.getObjectType("java/lang/String"), cst);
            } else if (cst instanceof Type) {
                Type type = (Type) cst;
                int sort = type.getSort();
                if (sort == Type.OBJECT || sort == Type.ARRAY) {
                    Type clazz = sort == Type.ARRAY ? type.getElementType() : type;
                    if (clazz.getSort() == Type.OBJECT) logdep.invoke(CLASS_KW, clazz.getInternalName());
                    return newValue(Type.getObjectType("java/lang/Class"));
                } else if (sort == Type.METHOD) {
                    return newValue(Type
                            .getObjectType("java/lang/invoke/MethodType"));
                } else {
                    throw new IllegalArgumentException("Illegal LDC constant "
                            + cst);
                }
            } else if (cst instanceof Handle) {
                return newValue(Type
                        .getObjectType("java/lang/invoke/MethodHandle"));
            } else {
                throw new IllegalArgumentException("Illegal LDC constant "
                        + cst);
            }
        case JSR:
            return RETURNADDRESS_VALUE;
        case GETSTATIC:
            logdep(((FieldInsnNode) insn).owner);
            return newValue(Type.getType(((FieldInsnNode) insn).desc));
        case NEW:
            return newValue(Type.getObjectType(((TypeInsnNode) insn).desc));
        default:
            throw new Error("Internal error.");
        }
    }

    @Override
    public Value copyOperation(final AbstractInsnNode insn,
            final Value value) throws AnalyzerException {
        return value;
    }

    @Override
    public Value unaryOperation(final AbstractInsnNode insn,
            final Value value) throws AnalyzerException {
        switch (insn.getOpcode()) {
        case INEG:
        case IINC:
        case L2I:
        case F2I:
        case D2I:
        case I2B:
        case I2C:
        case I2S:
            return INT_VALUE;
        case FNEG:
        case I2F:
        case L2F:
        case D2F:
            return FLOAT_VALUE;
        case LNEG:
        case I2L:
        case F2L:
        case D2L:
            return LONG_VALUE;
        case DNEG:
        case I2D:
        case L2D:
        case F2D:
            return DOUBLE_VALUE;
        case IFEQ:
        case IFNE:
        case IFLT:
        case IFGE:
        case IFGT:
        case IFLE:
        case TABLESWITCH:
        case LOOKUPSWITCH:
        case IRETURN:
        case LRETURN:
        case FRETURN:
        case DRETURN:
        case ARETURN:
            return null;
        case PUTSTATIC:
            logdep(((FieldInsnNode) insn).owner);
            return null;
        case GETFIELD:
            logdep(((FieldInsnNode) insn).owner);
            return newValue(Type.getType(((FieldInsnNode) insn).desc));
        case NEWARRAY:
            switch (((IntInsnNode) insn).operand) {
            case T_BOOLEAN:
                return newValue(Type.getType("[Z"));
            case T_CHAR:
                return newValue(Type.getType("[C"));
            case T_BYTE:
                return newValue(Type.getType("[B"));
            case T_SHORT:
                return newValue(Type.getType("[S"));
            case T_INT:
                return newValue(Type.getType("[I"));
            case T_FLOAT:
                return newValue(Type.getType("[F"));
            case T_DOUBLE:
                return newValue(Type.getType("[D"));
            case T_LONG:
                return newValue(Type.getType("[J"));
            default:
                throw new AnalyzerException(insn, "Invalid array type");
            }
        case ANEWARRAY:
            String desc = ((TypeInsnNode) insn).desc;
            return newValue(Type.getType("[" + Type.getObjectType(desc)));
        case ARRAYLENGTH:
            return INT_VALUE;
        case ATHROW:
            return null;
        case CHECKCAST:
            desc = ((TypeInsnNode) insn).desc;
            Type type = Type.getObjectType(desc);
            if (null == (TYPE_KW.invoke(value))) {
                logdep.invoke(ROOTCLASS_KW, type.getClassName());
            } else {
                logdep.invoke(CLASS_KW, type.getClassName());
            }
            return value;
        case INSTANCEOF:
            desc = ((TypeInsnNode) insn).desc;
            type = Type.getObjectType(desc);
            if (null == (TYPE_KW.invoke(value))) {
                logdep.invoke(ROOTCLASS_KW, type.getClassName());
            } else {
                logdep.invoke(CLASS_KW, type.getClassName());
            }
            return INT_VALUE;
        case MONITORENTER:
        case MONITOREXIT:
        case IFNULL:
        case IFNONNULL:
            return null;
        default:
            throw new Error("Internal error.");
        }
    }

    @Override
    public Value binaryOperation(final AbstractInsnNode insn,
            final Value value1, final Value value2)
            throws AnalyzerException {
        switch (insn.getOpcode()) {
        case IALOAD:
        case BALOAD:
        case CALOAD:
        case SALOAD:
        case IADD:
        case ISUB:
        case IMUL:
        case IDIV:
        case IREM:
        case ISHL:
        case ISHR:
        case IUSHR:
        case IAND:
        case IOR:
        case IXOR:
            return INT_VALUE;
        case FALOAD:
        case FADD:
        case FSUB:
        case FMUL:
        case FDIV:
        case FREM:
            return FLOAT_VALUE;
        case LALOAD:
        case LADD:
        case LSUB:
        case LMUL:
        case LDIV:
        case LREM:
        case LSHL:
        case LSHR:
        case LUSHR:
        case LAND:
        case LOR:
        case LXOR:
            return LONG_VALUE;
        case DALOAD:
        case DADD:
        case DSUB:
        case DMUL:
        case DDIV:
        case DREM:
            return DOUBLE_VALUE;
        case AALOAD:
            return REFERENCE_VALUE;
        case LCMP:
        case FCMPL:
        case FCMPG:
        case DCMPL:
        case DCMPG:
            return INT_VALUE;
        case IF_ICMPEQ:
        case IF_ICMPNE:
        case IF_ICMPLT:
        case IF_ICMPGE:
        case IF_ICMPGT:
        case IF_ICMPLE:
        case IF_ACMPEQ:
        case IF_ACMPNE:
            return null;
        case PUTFIELD:
            logdep(((FieldInsnNode) insn).owner);
            return null;
        default:
            throw new Error("Internal error.");
        }
    }

    @Override
    public Value ternaryOperation(final AbstractInsnNode insn,
            final Value value1, final Value value2,
            final Value value3) throws AnalyzerException {
        return null;
    }

   @Override
   public Value naryOperation(final AbstractInsnNode insn,
            final List values) throws AnalyzerException {
        int opcode = insn.getOpcode();
        if (opcode == MULTIANEWARRAY) {
            Type atype = Type.getType(((MultiANewArrayInsnNode) insn).desc);
            logdep(atype.getElementType());
            return newValue(atype);
        }
        if (opcode == INVOKEDYNAMIC) {
            InvokeDynamicInsnNode indy = (InvokeDynamicInsnNode) insn;
            logdep(Type.getReturnType(indy.desc));
            logdep(indy.bsm.getOwner());
            return newValue(Type.getReturnType(indy.desc));
        }
        MethodInsnNode minsn = (MethodInsnNode) insn;
        logdep(minsn.owner);
        for(Type arg: Type.getArgumentTypes(minsn.desc)) {
            logdep(arg);                
        }
        Type ret = Type.getReturnType(minsn.desc);
        logdep(ret);
        if (opcode == INVOKESPECIAL) {
            return newValue(ret);
        } else {
            return (Value) invoke.invoke(opcode == INVOKESTATIC, minsn.owner, minsn.name, minsn.desc, values);
        }
    }

    @Override
    public void returnOperation(final AbstractInsnNode insn,
            final Value value, final Value expected)
            throws AnalyzerException {
    }

    @Override
    public Value merge(final Value v, final Value w) {
        return (Value) merge.invoke(v, w);
    }
}
