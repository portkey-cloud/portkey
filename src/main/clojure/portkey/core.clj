(ns portkey.core
  (:require
    [portkey.ouroboros :as ou]
    [portkey.kryo :as kryo]
    [portkey.logdep :refer [log-dep *log-dep*]]))



; code to generate boiler plate
#_(for [method (.getMethods org.objectweb.asm.MethodVisitor)
       :when (and (re-matches #"visit.*Insn" (.getName method))
               (not (#{"visitLdcInsn" "visitMethodInsn"} (.getName method))))
       :let [name (symbol (.getName method))
             args (repeatedly (.getParameterCount method) gensym)]]
   `(~name [~@args]
      (reset! ~'strs [])))

(defn bytecode [class]
  (or (get (ou/bytecode [class]) class)
    (throw (ex-info "Can't find" {:class class}))))

(def primitive? #{"void" "int" "byte" "short" "long" "char" "boolean" "float" "double"})

(defn inspect-class [class]
  (let [classloader (.getClassLoader class)
       log-classname #(when-not (primitive? %) (log-dep :class (Class/forName (.replace ^String % \/ \.) false classloader)))
       bytes (bytecode class)
       rdr (org.objectweb.asm.ClassReader. bytes)
       class-visitor
       (proxy [org.objectweb.asm.ClassVisitor] [org.objectweb.asm.Opcodes/ASM4]
         (visit [version access name sig supername ifaces]
           (log-classname supername)
           (doseq [iface ifaces]
             (log-classname iface)))
         (visitField [access name desc sig value]
           (log-classname (.getClassName (org.objectweb.asm.Type/getType desc)))
           nil)
         (visitMethod [access method-name mdesc sig exs]
           (let [strs (atom [])
                 mtype (org.objectweb.asm.Type/getMethodType mdesc)]
             (log-classname (.getClassName (.getReturnType mtype)))
             (doseq [type (.getArgumentTypes mtype)]
               (log-classname (.getClassName type)))
             (doseq [ex exs]
               (log-classname ex))
             (proxy [org.objectweb.asm.MethodVisitor] [org.objectweb.asm.Opcodes/ASM4]
               (visitLdcInsn [x]
                 (if (string? x)
                   (swap! strs conj x)
                   (reset! strs [])))
               (visitMethodInsn [opcode ^String owner name desc itf]
                 (cond
                   (and (= opcode org.objectweb.asm.Opcodes/INVOKESTATIC)
                     (= owner "clojure/lang/RT")
                     (= name "var")
                     (= desc "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;")
                     (<= 2 (count @strs))) ; TODO warn when less than 2
                   (do
                     (log-dep :var-ref (symbol (peek (pop @strs)) (peek @strs)))
                     (reset! strs []))
                   (or (= opcode org.objectweb.asm.Opcodes/INVOKESTATIC)
                     (and (= opcode org.objectweb.asm.Opcodes/INVOKESPECIAL) (= "<init>" name)))
                   (log-classname owner)))
               (visitInsn [G__4852] (clojure.core/reset! strs [])) (visitIntInsn [G__4853 G__4854] (clojure.core/reset! strs [])) (visitFieldInsn [G__4855 G__4856 G__4857 G__4858] (clojure.core/reset! strs [])) (visitVarInsn [G__4859 G__4860] (clojure.core/reset! strs [])) (visitIincInsn [G__4861 G__4862] (clojure.core/reset! strs [])) (visitJumpInsn [G__4863 G__4864] (clojure.core/reset! strs [])) (visitTableSwitchInsn [G__4865 G__4866 G__4867 G__4868] (clojure.core/reset! strs [])) (visitLookupSwitchInsn [G__4869 G__4870 G__4871] (clojure.core/reset! strs [])) (visitInvokeDynamicInsn [G__4872 G__4873 G__4874 G__4875] (clojure.core/reset! strs [])) (visitTypeInsn [G__4876 G__4877] (clojure.core/reset! strs [])) (visitMultiANewArrayInsn [G__4878 G__4879] (clojure.core/reset! strs []))))))]
   (.accept rdr class-visitor 0)))

(def default-whitelist 
  #(if (var? %)
     (some-> % meta :ns ns-name (= 'clojure.core))
     (re-matches #"(?:clojure\.lang|java)\..*" (.getName %))))

(defn package 
  ([root]
    (package root default-whitelist))
  ([root whitelist?]
    (let [deps (atom [])]
      (binding [*log-dep* #(swap! deps conj %)]
        (let [root-bytes (kryo/freeze root)]
          (loop [todo (set @deps) vars {} classes #{}]
            (reset! deps [])
            (if-some [dep (first todo)]
              (let [todo (disj todo dep)]
                (cond
                  (or (whitelist? dep) (vars dep) (classes dep)) (recur todo vars classes)
                  (var? dep)
                  (let [bytes (kryo/freeze @dep)]
                    (recur (into todo @deps) (assoc vars dep bytes) classes))
                  (class? dep)
                  (do
                    (inspect-class dep)
                    (recur (into todo @deps) vars (conj classes dep)))))
              {:vars vars :classes classes :root root-bytes})))))))

(defn bootstrap
  "Returns a serialized thunk (0-arg fn). This thunk when called returns deserialized root with all vars set."
  [{:keys [root vars]}]
  (kryo/freeze
    (fn []
     (doseq [[^clojure.lang.Var v bs] vars]
       (.bindRoot v (kryo/unfreeze bs)))
     (kryo/unfreeze root))))

#_(def stub
   (reify com.amazonaws.services.lambda.runtime.RequestStreamHandler
     (handleRequest [_ in out ctx]
       (prn in out ctx))))
