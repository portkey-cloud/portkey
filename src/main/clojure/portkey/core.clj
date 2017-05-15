(ns portkey.core
  (:require
    [portkey.ouroboros :as ou]
    [cemerick.pomegranate.aether :as mvn]
    [portkey.kryo :as kryo]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [cheshire.core :as json]
    [portkey.logdep :refer [log-dep *log-dep*]]))

; code to generate boiler plate
#_(for [method (.getMethods org.objectweb.asm.MethodVisitor)
       :when (and (re-matches #"visit.*Insn" (.getName method))
               (not (#{"visitLdcInsn" "visitMethodInsn"} (.getName method))))
       :let [name (symbol (.getName method))
             args (repeatedly (.getParameterCount method) gensym)]]
   `(~name [~@args]
      (reset! ~'strs [])))

(defn- resource-name [^Class class]
  (str (.replace (.getName class) \. \/) ".class"))

(defn- get-bytes [in]
  (let [bos (java.io.ByteArrayOutputStream.)]
    (with-open [in (io/input-stream in)]
      (io/copy in bos))
    (.toByteArray bos)))

(defn bytecode [class]
  (or
    (some-> (.getClassLoader class) (.getResource (resource-name class)) get-bytes)
    (get (ou/bytecode [class]) class)
    (throw (ex-info "Can't find" {:class class}))))

(def primitive? #{"void" "int" "byte" "short" "long" "char" "boolean" "float" "double"})

(defn inspect-class [^Class class]
  (let [classloader (.getClassLoader class)
       log-classname #(when-some [classname (if-some [[_ t] (re-matches #"\[+(?:[ZBCDFIJS]|L(.*);)" %)]
                                              t
                                              (when-not (primitive? %)
                                                (.replace ^String % \/ \.)))]
                        (when-some [class (try (Class/forName classname false classloader)
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
         (visitField [access name ^String desc sig value]
           (log-classname (.getClassName (org.objectweb.asm.Type/getType desc)))
           nil)
         (visitMethod [access method-name mdesc sig exs]
           (let [strs (atom [])
                 mtype (org.objectweb.asm.Type/getMethodType mdesc)]
             (log-classname (.getClassName (.getReturnType mtype)))
             (doseq [^org.objectweb.asm.Type type (.getArgumentTypes mtype)]
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
                     (= owner "java/lang/Class")
                     (= name "forName"))
                   (binding [*out* *err*] (println "Blinded by reflection in" class method-name mdesc))
                   (and (= opcode org.objectweb.asm.Opcodes/INVOKESTATIC)
                     (= owner "clojure/lang/RT")
                     (= name "var")
                     (= desc "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;")
                     (<= 2 (count @strs))) ; TODO warn when less than 2
                   (log-dep :var-ref (symbol (peek (pop @strs)) (peek @strs)))
                   (and (= opcode org.objectweb.asm.Opcodes/INVOKESTATIC)
                     (= owner "clojure/lang/RT")
                     (= name "classForName")
                     (= desc "(Ljava/lang/String;)Ljava/lang/Class;")
                     (<= 1 (count @strs)))
                   (log-classname (peek @strs))
                   (or (= opcode org.objectweb.asm.Opcodes/INVOKESTATIC)
                     (and (= opcode org.objectweb.asm.Opcodes/INVOKESPECIAL) (= "<init>" name)))
                   (log-classname owner))
                 (reset! strs []))
               (visitFieldInsn [opcode owner name desc]
                 (when (or (= opcode org.objectweb.asm.Opcodes/GETSTATIC)
                         (= opcode org.objectweb.asm.Opcodes/PUTSTATIC))
                   (log-classname owner))
                 (reset! strs []))
               (visitInsn [G__4852] (clojure.core/reset! strs [])) (visitIntInsn [G__4853 G__4854] (clojure.core/reset! strs [])) (visitVarInsn [G__4859 G__4860] (clojure.core/reset! strs [])) (visitIincInsn [G__4861 G__4862] (clojure.core/reset! strs [])) (visitJumpInsn [G__4863 G__4864] (clojure.core/reset! strs [])) (visitTableSwitchInsn [G__4865 G__4866 G__4867 G__4868] (clojure.core/reset! strs [])) (visitLookupSwitchInsn [G__4869 G__4870 G__4871] (clojure.core/reset! strs [])) (visitInvokeDynamicInsn [G__4872 G__4873 G__4874 G__4875] (clojure.core/reset! strs [])) (visitTypeInsn [G__4876 G__4877] (clojure.core/reset! strs [])) (visitMultiANewArrayInsn [G__4878 G__4879] (clojure.core/reset! strs []))))))]
   (.accept rdr class-visitor 0)))

(defn- bootstrap-class? [^Class class]
  (if-some [cl (.getClassLoader class)]
    (identical? cl (.getClassLoader Class)) ; in case a JVM does not return null for boostrap
    true))

(def default-whitelist
  #(if (var? %)
     (some-> % meta :ns ns-name #{'clojure.core 'portkey.logdep 'portkey.kryo 'carbonite.serializer})
     (or (bootstrap-class? %)
       (re-matches #"(?:clojure\.(?:lang\.|java\.|core\$)|com\.esotericsoftware\.kryo\.).*" (.getName ^Class %)))))

(defn bom
  "Computes the bill-of-materials for an object."
  ([root]
    (bom root default-whitelist))
  ([root whitelist?]
    (let [deps (atom [])]
      (binding [*log-dep* #(when-not (whitelist? %) (swap! deps conj %))]
        (let [root-bytes (kryo/freeze root)]
          (loop [todo #{} vars {} classes #{}]
            (let [todo (into todo (comp (remove vars) (remove classes)) @deps)]
              (reset! deps [])
              (if-some [dep (first todo)]
                (let [todo (disj todo dep)]
                  (cond
                    (var? dep)
                    (let [bytes (kryo/freeze @dep)]
                      (recur todo (assoc vars dep bytes) classes))
                    (class? dep)
                    (do
                      (inspect-class dep)
                      (recur todo vars (conj classes dep)))))
                {:vars vars :classes classes :root root-bytes}))))))))

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
                        (map #(java.io.File. (.toURI ^java.net.URL %)))
                        (filter (fn [^java.io.File f] (and (.isFile f) (.endsWith (.getName f) ".jar")))))
                      (.getURLs ^java.net.URLClassLoader (java.lang.ClassLoader/getSystemClassLoader)))
        coords (into []
                 (comp
                   (map #(.getName ^java.io.File %))
                   (keep #(re-matches #"(kryo|clojure|carbonite)-(\d+\.\d+\.\d+(?:-.*)?)\.jar" %))
                   (map (fn [[_ p v]]
                          (cond-> [(symbol ({"clojure" "org.clojure"
                                            "kryo" "com.esotericsoftware"
                                            "carbonite" "com.twitter"} p) p) v]
                            (= p "carbonite") (into '[:exclusions [com.esotericsoftware.kryo/kryo]])))))
                 system-jars)]
    (into {}
      (map (fn [^java.io.File file] [(str "lib/" (.getName file)) file]))
      (mvn/dependency-files (mvn/resolve-dependencies :retrieve true :coordinates coords
                              :repositories (assoc mvn/maven-central "clojars" "http://clojars.org/repo"))))))

(defn class-entries [classes]
  (into {}
    (for [class classes]
      [(resource-name class) (bytecode class)])))

(def ^:private support-entries
  (-> support-deps
    (into (for [clj ["portkey/logdep.clj" "portkey/kryo.clj"]]
            [clj (.getResource (.getContextClassLoader (Thread/currentThread)) clj)]))
    (into (class-entries (conj (:classes (bom portkey.LambdaStub)) portkey.SerializerStub)))))

(defn package!
  "Writes f as AWS Lambda deployment packge to out.
   f must be a function of 3 arguments: input, output and context. (See RequestHandler)
   Additionally, more classes and vars can be specified, focring them to be kept in the
   package."
  [out f & keeps]
  (let [fbom (bom f)
        bom (transduce (comp (map bom) (map #(dissoc % :root)))
              (partial merge-with into) fbom keeps)
        {:keys [classes root]} (bootstrap bom)
        entries (-> support-entries
                  (assoc "bootstrap.kryo" root)
                  (into (class-entries classes)))]
    (zip! out entries)
    out))

(defn- atomic? [class]
  (or (.isAssignableFrom Number class)
    (= String class)
    (= Boolean class)))

(defn- as-doto* [^Class class m]
  (let [setters (reduce (partial merge-with into) {}
                  (for [m (.getMethods class)
                        :let [name (.getName m)
                              params (.getParameterTypes m)]
                        :when (.startsWith name "set")
                        :when (= 1 (count params))]
                    {name #{(aget params 0)}}))
        setter-call (fn [[k v]]
                      (let [mname (str/replace (str "set-" (name k)) #"-(.)" (fn [[_ ^String c]] (.toUpperCase c)))
                            types (setters mname)]
                        (if (map? v)
                          (let [types (remove atomic? types)]
                            (case (count types)
                              0 (throw (IllegalStateException. (str "No bean for method " mname " on class " class)))
                              1 `(~(symbol (str "." mname)) ~(as-doto* (first types) v))
                              (throw (IllegalStateException. (str "Too many overrides for method " mname " on class " class)))))
                          `(~(symbol (str "." mname)) ~v))))]
    `(doto (new ~class)
       ~@(map setter-call m))))

(defn- aws-name-munge [name]
  (str/replace name #"(_)|(\.)|(/)|([^a-zA-Z0-9-_])"
    (fn [[_ underscore dot slash other]]
      (cond
        underscore "__"
        dot "_-"
        slash "_I"
        :else (format "_u%04X" (int (.charAt ^String other 0)))))))

(defn- aws-name-unmunge [name]
  (str/replace name #"_(?:(_)|(-)|(I)|u([0-9a-fA-F]{4}))"
    (fn [[_ underscore dot slash other]]
      (cond
        underscore "_"
        dot "."
        slash "/"
        :else (char (Long/parseLong other 16))))))

(defmacro ^:private donew [class m]
  (as-doto* (resolve class) m))

(def build
  (memoize
   (fn [cls]
     (let [client (eval (list (symbol (.getCanonicalName cls) "defaultClient")))]
       (-> (Runtime/getRuntime)
           (.addShutdownHook (Thread. #(.shutdown client))))
       client))))

(defn make-function-name [f]
  (-> f class .getCanonicalName (str/replace "$" "/") aws-name-munge))

(defn deploy! [f]
  (let [bb (-> (java.io.ByteArrayOutputStream.)
               (doto (package! f))
               .toByteArray
               java.nio.ByteBuffer/wrap)
        function-name (make-function-name f)]
    (if (->> (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
             (.listFunctions)
             (.getFunctions)
             (filter #(= function-name (.getFunctionName %)))
             empty?)
      (when (->> (build com.amazonaws.services.identitymanagement.AmazonIdentityManagementClientBuilder)
                 (.listRoles)
                 (.getRoles)
                 (filter #(= "portkey" %))
                 empty?)
        (let [role (-> (build com.amazonaws.services.identitymanagement.AmazonIdentityManagementClientBuilder)
                       (.createRole (donew com.amazonaws.services.identitymanagement.model.CreateRoleRequest
                                           {:role-name "portkey"
                                            :assume-role-policy-document (json/generate-string {:Version "2012-10-17"
                                                                                                :Statement [{:Effect :Allow
                                                                                                             :Principal {:Service "lambda.amazonaws.com"}
                                                                                                             :Action "sts:AssumeRole"}]})})))]
          (-> (build com.amazonaws.services.identitymanagement.AmazonIdentityManagementClientBuilder)
              (.putRolePolicy (donew com.amazonaws.services.identitymanagement.model.PutRolePolicyRequest
                                     {:role-name "portkey"
                                      :policy-name "portkey_basic_execution"
                                      :policy-document (json/generate-string {:Version "2012-10-17"
                                                                              :Statement [{:Effect :Allow
                                                                                           :Action "logs:CreateLogGroup"
                                                                                           :Resource "arn:aws:logs:*:*:*"}
                                                                                          {:Effect "Allow"
                                                                                           :Action ["logs:CreateLogStream" "logs:PutLogEvents"]
                                                                                           :Resource ["arn:aws:logs:*:*:log-group:*:*"]}]})})))
          (.createFunction (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
                           (donew com.amazonaws.services.lambda.model.CreateFunctionRequest
                                  {:function-name function-name
                                   :handler "portkey.LambdaStub"
                                   :code {:zip-file bb}
                                   :role (-> role .getRole .getArn)
                                   :runtime "java8"}))))
      (.updateFunctionCode (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
                           (donew com.amazonaws.services.lambda.model.UpdateFunctionCodeRequest
                                  {:function-name function-name
                                   :zip-file bb})))))

(defn invoke [f]
  (.invoke (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
           (donew com.amazonaws.services.lambda.model.InvokeRequest
                  {:function-name (make-function-name f)})))


#_(deployment-package prn)

#_(def stub
   (reify com.amazonaws.services.lambda.runtime.RequestStreamHandler
     (handleRequest [_ in out ctx]
       (prn in out ctx))))

