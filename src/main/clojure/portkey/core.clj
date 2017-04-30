(ns portkey.core
  (:require
    [portkey.ouroboros :as ou]
    [cemerick.pomegranate.aether :as mvn]
    [portkey.kryo :as kryo]
    [clojure.java.io :as io]
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
       log-classname #(cond
                        (.endsWith % "[]") (recur (subs % 0 (- (count %) 2)))
                        (not (primitive? %)) (when-some [class (try (Class/forName (.replace ^String % \/ \.) false classloader)
                                                                 (catch ClassNotFoundException _))]
                                               (log-dep :class class)))
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
     (some-> % meta :ns ns-name #{'clojure.core 'portkey.logdep 'portkey.kryo 'carbonite.serializer})
     (re-matches #"(?:clojure\.(?:lang\.|java\.|core\$)|java\.|com\.esotericsoftware\.kryo\.).*" (.getName %))))

(defn bom
  "Computes the bill-of-materials for an object."
  ([root]
    (bom root default-whitelist))
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
  [{:keys [root vars classes]}]
  (let [bom' (bom
               (fn []
                 (doseq [[^clojure.lang.Var v bs] vars]
                   (.bindRoot v (kryo/unfreeze bs)))
                 (kryo/unfreeze root)))]
    (when-some [new-vars (seq (remove vars (:vars bom')))]
      (throw (ex-info "The boostrap function shouldn't use any new var." {:new-vars new-vars})))
    (update bom' :classes into classes)))

(defn zip! 
  "Writes a zip to out."
  [out entries]
  (let [entries-map (into (sorted-map) entries)]
    (with-open [out (io/output-stream out)
                zip (java.util.zip.ZipOutputStream. out)]
      (letfn [(^String emit-dirs [^String dir ^String path]
                (if-not (.startsWith path dir)
                  (recur (subs dir 0 (inc (.lastIndexOf dir "/" (- (count dir) 2)))) path)
                  (let [i (.indexOf path "/" (count dir))]
                    (if (neg? i)
                      dir
                      (let [dir (subs path 0 (inc i))]
                        (.putNextEntry zip (java.util.zip.ZipEntry. dir))
                        (recur dir path))))))]
        (reduce-kv (fn [^String dir ^String path data]
                     (let [dir (emit-dirs dir path)]
                       (.putNextEntry zip (java.util.zip.ZipEntry. path))
                       (with-open [in (io/input-stream data)] (io/copy in zip))
                       dir)) "" entries-map)))))


(def ^:private support-deps
  (let [system-jars (into #{}
                      (comp
                        (map #(java.io.File. (.toURI %)))
                        (filter #(and (.isFile %) (.endsWith (.getName %) ".jar"))))
                      (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))
        coords (into []
                 (comp
                   (map #(.getName %))
                   (keep #(re-matches #"(kryo|clojure|carbonite)-(\d+\.\d+\.\d+(?:-.*)?)\.jar" %))
                   (map (fn [[_ p v]]
                          (cond-> [(symbol ({"clojure" "org.clojure"
                                            "kryo" "com.esotericsoftware"
                                            "carbonite" "com.twitter"} p) p) v]
                            (= p "carbonite") (into '[:exclusions [com.esotericsoftware.kryo/kryo]])))))
                 system-jars)]
    (into {}
      (map (fn [file] [(str "lib/" (.getName file)) file]))
      (mvn/dependency-files (mvn/resolve-dependencies :retrieve true :coordinates coords
                              :repositories (assoc mvn/maven-central "clojars" "http://clojars.org/repo"))))))

(defn class-entries [classes]
  (into {}
    (for [^Class class classes]
      [(str (.replace (or (.getCanonicalName class) (.getName class)) \. \/) ".class") (bytecode class)])))

(def ^:private support-entries
  (-> support-deps
    (into (for [clj ["portkey/logdep.clj" "portkey/kryo.clj"]]
            [clj (.getResource (.getContextClassLoader (Thread/currentThread)) clj)]))
    (into (class-entries (conj (:classes (bom portkey.LambdaStub)) portkey.SerializerStub)))))

(defn package!
  "Writes f as AWS Lambda deployment packge to out.
   f must be a function of 3 arguments: input, output and context. (See RequestHandler)"
  [out f]
  (let [{:keys [classes root]} (bootstrap (bom f))
        entries (-> support-entries
                  (assoc "bootstrap.kryo" root)
                  (into (class-entries classes)))]
    (zip! out entries)
    out))

#_(deployment-package prn)

#_(def stub
   (reify com.amazonaws.services.lambda.runtime.RequestStreamHandler
     (handleRequest [_ in out ctx]
       (prn in out ctx))))

