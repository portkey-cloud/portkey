(ns portkey.core
  (:require
    [portkey.ouroboros :as ou]
    [cemerick.pomegranate.aether :as mvn]
    [portkey.kryo :as kryo]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [cheshire.core :as json]
    [portkey.logdep :refer [log-dep]]
    [portkey.aws :as aws]))

(def ^:dynamic ^ClassLoader *classloader* nil)

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

(def ^:private ^:dynamic *source*)

(defn inspect-class [^Class class]
  (try
    (binding [*classloader* (.getClassLoader class)
              *source* class]
      (let [log-classname #(log-dep :class %)
            bytes (bytecode class)
            rdr (org.objectweb.asm.ClassReader. bytes)
            class-node (doto (org.objectweb.asm.tree.ClassNode.)
                         (as-> cn  (.accept rdr cn 0)))
            analyzer (org.objectweb.asm.tree.analysis.Analyzer. (portkey.analysis.UCInterpreter.))]
        (log-classname (.superName class-node))
        (doseq [iface (.interfaces class-node)]
          (log-classname iface))
        (doseq [^org.objectweb.asm.tree.FieldNode field (.fields class-node)]
          (log-classname (.getClassName (org.objectweb.asm.Type/getType (.desc field)))))
        (doseq [^org.objectweb.asm.tree.MethodNode method (.methods class-node)
                :let [mtype (org.objectweb.asm.Type/getMethodType (.desc method))]]
          (log-classname (.getClassName (.getReturnType mtype)))
          (doseq [^org.objectweb.asm.Type type (.getArgumentTypes mtype)]
            (log-classname (.getClassName type)))
          (run! log-classname (.exceptions method))
          (.analyze analyzer (.name class-node) method))))
    (catch Throwable t
      (throw (Exception. (str "Failed to inspect class: " (.getName class)) t)))))

(defn- bootstrap-class? [^Class class]
  (if-some [cl (.getClassLoader class)]
    (identical? cl (.getClassLoader Class)) ; in case a JVM does not return null for boostrap
    true))

(def whitelist-nses
  '#{clojure.core
     clojure.walk
     clojure.zip
     clojure.java.api
     clojure.java.browse
     clojure.java.io
     clojure.java.javadoc
     clojure.java.shell
     clojure.pprint
     clojure.string
     portkey.logdep portkey.kryo portkey.ouroboros carbonite.serializer})

(def white-list-pattern
  (re-pattern
    (str "(?:clojure\\.lang\\.|"
      (clojure.string/join "|"
        (map #(str (java.util.regex.Pattern/quote (name %)) "[$.]")
          whitelist-nses))
      "|com\\.esotericsoftware\\.kryo\\."
      "|com\\.sun\\.tools\\."
      ").*")))

(def default-whitelist
  #(if (var? %)
     (some-> % meta :ns ns-name whitelist-nses)
     (or (bootstrap-class? %)
       (re-matches white-list-pattern (.getName ^Class %)))))

(def primitive? #{"void" "int" "byte" "short" "long" "char" "boolean" "float" "double"})

(defn- ensure-class [x]
  (cond
    (string? x)
    (when-some [classname (if-some [[_ t] (re-matches #"\[+(?:[ZBCDFIJS]|L(.*);)" x)]
                             t
                             (when-not (primitive? x)
                               (.replace ^String x \/ \.)))] 
      (when-some [class (try (Class/forName classname false *classloader*)
                          (catch ClassNotFoundException _)
                          (catch java.lang.IncompatibleClassChangeError _
                            (prn 'java.lang.IncompatibleClassChangeError x))
                          (catch java.lang.NoClassDefFoundError _))]
        class))
    (class? x) x))

(defn dep-logger [log!]
  (fn [type x]
    (case type
      :var (log! :vars x)
      :class (some->> x ensure-class (log! :classes))
      :root-class (some->> x ensure-class ou/descendants (run! #(log! :classes %)))
      :var-ref (log! :vars (clojure.java.api.Clojure/var x))
      :fake (with-bindings {#_#_clojure.lang.Compiler/LOADER *classloader*}
              (load (str "/" x))
              (log! :fakes x))
      :resource (log! :resources (re-find #"[^/].*" x)))))

(defn- deref-and-set! [a x]
  (loop []
    (let [x' @a]
      (if (compare-and-set! a x' x)
        x'
        (recur)))))

(def ^:dynamic *debug-deps*)

(defn ^:private edge! [k to]
  (when (bound? #'*debug-deps*)
    (set! *debug-deps* (update *debug-deps* to (fnil conj #{}) *source*))))

(defmacro debug-deps [& body]
  `(binding [*debug-deps* {}]
     ~@body
     *debug-deps*))

(defn bom
  "Computes the bill-of-materials for an object."
  ([root]
    (bom root default-whitelist))
  ([root whitelist?]
    (let [empty-deps {:fakes #{} :classes #{} :vars #{} :resources #{}}
          deps (atom empty-deps)
          empty-bom {:fakes #{} :classes #{} :vars {} :resources #{} :requires #{}}]
      (binding [log-dep (dep-logger #(do
                                       (edge! %1 %2)
                                       (swap! deps update %1 conj %2)))]
        (loop [bom {:root (binding [*source* :root] (kryo/freeze root))
                    :fakes #{} :classes #{} :vars {} :resources #{} :requires #{}}]
          (let [{:keys [fakes classes vars resources] :as new-deps} (deref-and-set! deps empty-deps)]
            (if (identical? new-deps empty-deps)
              bom
              (-> bom
                (update :requires into (comp (filter whitelist?) (map #(-> % meta :ns ns-name))) vars)
                (update :classes into (comp
                                        (remove whitelist?)
                                        (remove (:classes bom))
                                        (map #(doto % inspect-class))) classes)
                (update :resources into resources)
                (update :fakes into fakes)
                (update :vars into (comp (remove whitelist?) (remove (:vars bom))
                                     (map (fn [v]
                                            [v (binding [*source* v]
                                                 (kryo/freeze [(:dynamic (meta v)) @v]))])))
                  vars)
                recur))))))))

(defn bootstrap
  "Returns a serialized thunk (0-arg fn). This thunk when called returns deserialized root with all vars set."
  [{:keys [root vars classes fakes requires]}]
  (let [bom' (bom
               (fn []
                 (apply require requires)
                 (doseq [[^clojure.lang.Var v bs] vars]
                   (let [[d root] (kryo/unfreeze bs)]
                     (.bindRoot v root)
                     (when d
                       (alter-meta! v assoc :dynamic true)
                       (.setDynamic v))))
                 (kryo/unfreeze root)))]
    (when-some [new-vars (seq (remove vars (:vars bom')))]
      (throw (ex-info "The boostrap function shouldn't use any new var." {:new-vars new-vars})))
    (-> bom'
      (update :classes into classes)
      (assoc :fakes fakes))))

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

(defn- tmp-dir [prefix]
  (let [root (java.io.File. (System/getProperty "java.io.tmpdir"))
        base (str prefix "-" (System/currentTimeMillis) "-")]
    (loop [i 0]
      (let [dir (java.io.File. root (str base i))]
        (cond
          (.mkdir dir) dir
          (< i 100) (recur (inc i))
          :else (throw (IllegalStateException. (str "Can't create tmp dir prefixed by " (pr-str base) " after " i " collisions."))))))))

(defn- resource-url [path]
  (.getResource (.getContextClassLoader (Thread/currentThread)) path))

(def ^:private support-entries
  (-> support-deps
    (into (for [clj ["portkey/logdep.clj" "portkey/kryo.clj"]]
            [clj (resource-url clj)]))
    (into (class-entries (conj (:classes (bom portkey.LambdaStub)) portkey.SerializerStub)))))

(defn package!
  "Writes f as AWS Lambda deployment packge to out.
   f must be a function of 3 arguments: input, output and context. (See RequestHandler)
   Additionally, more classes and vars can be specified, forcing them to be kept in the
   package."
  [out f & keeps]
  (let [fbom (bom f)
        resources (into {}
                        (comp (filter string?)
                              (map (fn [path] [path (resource-url path)]))
                              (filter (fn [[path url]] url)))
                        (concat keeps (:resources fbom)))
        keeps (remove string? keeps)
        bom (transduce (comp (map bom) (map #(dissoc % :root)))
              (partial merge-with into) fbom keeps)
        {:keys [classes root fakes vars]} (bootstrap bom)
        entries (-> support-entries
                  (assoc "bootstrap.kryo" root)
                  (into resources)
                  (into (class-entries classes))
                  (into (zipmap (map #(str % ".clj") fakes) (repeat (byte-array 0)))))]
    (zip! out entries)
    out))

(defn- atomic? [class]
  (or (.isAssignableFrom Number class)
    (= String class)
    (= Boolean class)))

(defn- as-doto [^Class class & ops]
  (let [parse (fn parse [ops]
                (lazy-seq
                  (when-some [[op & ops] (seq ops)]
                    (if (keyword? op)
                      (case op
                        :when 
                        (let [[_ test m & ops] ops]
                          (cons [:when test m] (parse ops))))
                      (cons [:set op] (parse ops))))))
        ops (parse ops)
        doto-form
        (fn [m]
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
                                      1 `(~(symbol (str "." mname)) ~(as-doto (first types) v))
                                      (throw (IllegalStateException. (str "Too many overrides for method " mname " on class " class)))))
                                  `(~(symbol (str "." mname)) ~v))))]
            `(doto ~@(map setter-call m))))]
    (if (= java.util.Map class)
      `(-> {} ~@(for [[op a b] ops]
                  (case op
                    :set `(into ~a)
                    :when `(cond-> ~a (into ~b)))))
      `(-> (new ~(symbol (.getName class)))
         ~@(for [[op a b] ops]
             (case op
               :set (doto-form a)
               :when `(cond-> ~a ~(doto-form b))))))))

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

(defmacro ^:private donew [class & ms]
  (apply as-doto (resolve class) ms))

(defmacro ^:private doset [x m]
  (if-some [class (or
                    (some-> x meta :tag resolve)
                    (some-> m meta :tag resolve)
                    (some-> &form meta :tag resolve))]
    (as-doto class x m)
    (throw (ex-info "No :tag meta on forms passed to doset."
             {:forms [x m &form]}))))

(def build
  (memoize
   (fn [cls]
     (let [client (eval (list (symbol (.getCanonicalName cls) "defaultClient")))]
       (-> (Runtime/getRuntime)
           (.addShutdownHook (Thread. #(.shutdown client))))
       client))))

(defmacro try-some [& body]
  `(try
     (do ~@body)
     (catch Throwable t#
       nil)))

(def fetch-portkey-role
  (memoize
   (fn []
     (-> (build com.amazonaws.services.identitymanagement.AmazonIdentityManagementClientBuilder)
         (.getRole (donew com.amazonaws.services.identitymanagement.model.GetRoleRequest
                          {:role-name "portkey"}))))))

(defn fetch-api [api-name]
  (some-> (build com.amazonaws.services.apigateway.AmazonApiGatewayClientBuilder)
          (.getRestApis (com.amazonaws.services.apigateway.model.GetRestApisRequest.))
          (.getItems)
          (->> (filter #(= api-name (.getName %))))
          first))

(defn get-function-policy [lambda-function-name]
  (try
    (-> (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
        (.getPolicy (donew com.amazonaws.services.lambda.model.GetPolicyRequest
                           {:function-name lambda-function-name})))
    (catch com.amazonaws.services.lambda.model.ResourceNotFoundException e
      nil)))

(def get-function
  (memoize
   (fn [function-name]
     (-> (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
         (.getFunction (donew com.amazonaws.services.lambda.model.GetFunctionRequest
                              {:function-name function-name}))))))

(defn ensure-api [lambda-function-name api-function-name parsed-path]
  (let [function-configuration (-> (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
                                   (.getFunctionConfiguration (donew com.amazonaws.services.lambda.model.GetFunctionConfigurationRequest
                                                                     {:function-name lambda-function-name})))
        {:keys [region account]} (-> function-configuration
                                     .getFunctionArn
                                     aws/parse-arn)
        function-policy (get-function-policy lambda-function-name)]
    (if-let [id (some-> (fetch-api "portkey") (.getId))]
      (do
        (-> (build com.amazonaws.services.apigateway.AmazonApiGatewayClientBuilder)
            (.putRestApi (donew com.amazonaws.services.apigateway.model.PutRestApiRequest
                                {:rest-api-id id
                                 :body (-> (aws/swagger-doc api-function-name
                                                            (.getFunctionArn function-configuration)
                                                            parsed-path)
                                           cheshire.core/generate-string
                                           (.getBytes "UTF-8")
                                           java.nio.ByteBuffer/wrap)
                                 :fail-on-warnings true})))
        (when-not function-policy
          (-> (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
              (.addPermission (donew com.amazonaws.services.lambda.model.AddPermissionRequest
                                     {:function-name lambda-function-name
                                      :statement-id (str lambda-function-name "Execution")
                                      :action "lambda:InvokeFunction"
                                      :principal "apigateway.amazonaws.com"
                                      :source-arn (str "arn:aws:execute-api:"
                                                       region
                                                       ":"
                                                       account
                                                       ":"
                                                       id
                                                       "/*/*/*")}))))
        id)
      (let [id (-> (build com.amazonaws.services.apigateway.AmazonApiGatewayClientBuilder)
                   (.importRestApi (donew com.amazonaws.services.apigateway.model.ImportRestApiRequest
                                          {:body (-> (aws/swagger-doc api-function-name
                                                                      (.getFunctionArn function-configuration)
                                                                      parsed-path)
                                                     cheshire.core/generate-string
                                                     (.getBytes "UTF-8")
                                                     java.nio.ByteBuffer/wrap)
                                           :fail-on-warnings true}))
                   (.getId))]
        (-> (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
            (.addPermission (donew com.amazonaws.services.lambda.model.AddPermissionRequest
                                   {:function-name lambda-function-name
                                    :statement-id (str lambda-function-name "Execution")
                                    :action "lambda:InvokeFunction"
                                    :principal "apigateway.amazonaws.com"
                                    :source-arn (str "arn:aws:execute-api:"
                                                     region
                                                     ":"
                                                     account
                                                     ":"
                                                     id
                                                     "/*/*/*")})))
        id))))

(defn deploy-api! [id stage]
  (-> (build com.amazonaws.services.apigateway.AmazonApiGatewayClientBuilder)
      (.createDeployment (donew com.amazonaws.services.apigateway.model.CreateDeploymentRequest
                                {:stage-name stage
                                 :rest-api-id id}))))

(defn fetch-subnet-ids []
  (->> (build com.amazonaws.services.ec2.AmazonEC2ClientBuilder)
       (.describeSubnets)
       (.getSubnets)
       (mapv #(.getSubnetId %))))

(defn fetch-security-group-ids [security-group-names]
  (->> (build com.amazonaws.services.ec2.AmazonEC2ClientBuilder)
       (.describeSecurityGroups)
       (.getSecurityGroups)
       (filter #(security-group-names (.getGroupName %)))
       (mapv #(.getGroupId %))))

(defn parse-vpc-config [{:keys [subnet-ids security-group-ids security-groups]}]
  (cond-> {}
    (= subnet-ids :all) (assoc :subnet-ids (fetch-subnet-ids))
    security-group-ids (assoc :security-group-ids security-group-ids)
    security-groups (assoc :security-group-ids (fetch-security-group-ids (set security-groups)))))

(defn deploy! [f lambda-function-name & {:keys [keeps environment-variables vpc-config]}]
  (let [bb (-> (java.io.ByteArrayOutputStream.)
               (doto (package! f keeps))
               .toByteArray
               java.nio.ByteBuffer/wrap)]
    (when-not (try-some (fetch-portkey-role))
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
                                    :policy-name "lambda_execution"
                                    :policy-document (json/generate-string {:Version "2012-10-17"
                                                                            :Statement [{:Effect :Allow
                                                                                         :Action "logs:CreateLogGroup"
                                                                                         :Resource "arn:aws:logs:*:*:*"}
                                                                                        {:Effect "Allow"
                                                                                         :Action ["logs:CreateLogStream"
                                                                                                  "logs:PutLogEvents"]
                                                                                         :Resource "arn:aws:logs:*:*:log-group:*:*"}
                                                                                        {:Effect "Allow"
                                                                                         :Action ["ec2:CreateNetworkInterface"
                                                                                                  "ec2:DescribeNetworkInterfaces"
                                                                                                  "ec2:DeleteNetworkInterface"
                                                                                                  "kms:Decrypt"]
                                                                                         :Resource "*"}]})})))))
    (if-not (try-some (get-function lambda-function-name))
      (let [arn (-> (try-some (fetch-portkey-role)) (.getRole) (.getArn))
            client (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
            req (donew com.amazonaws.services.lambda.model.CreateFunctionRequest
                  {:function-name lambda-function-name
                   :handler "portkey.LambdaStub"
                   :code {:zip-file bb}
                   :role arn
                   :runtime "java8"
                   :memory-size (int 1536)
                   :timeout (int 30)
                   :environment {:variables environment-variables}}
                  :when vpc-config
                  {:vpc-config (parse-vpc-config vpc-config)})]
        (let [{:keys [exception result]}
              (reduce (fn [acc sleep-time]
                        (try
                          (reduced {:result (.createFunction client req)})
                          (catch com.amazonaws.services.lambda.model.InvalidParameterValueException e
                            (if (.startsWith (.getMessage e) "The role defined for the function cannot be assumed by Lambda")
                              (do
                                (Thread/sleep sleep-time)
                                {:exception e})
                              (reduced {:exception e})))))
                      nil
                      (repeat 20 1000))]
          (if exception
            (throw exception)
            (.getFunctionArn result))))
      (let [arn (-> (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
                    (.updateFunctionCode (donew com.amazonaws.services.lambda.model.UpdateFunctionCodeRequest
                                                {:function-name lambda-function-name
                                                 :zip-file bb}))
                    (.getFunctionArn))]
        (-> (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
            (.updateFunctionConfiguration
              (donew com.amazonaws.services.lambda.model.UpdateFunctionConfigurationRequest
                {:function-name lambda-function-name
                 :environment {:variables environment-variables}}
                :when vpc-config
                {:vpc-config (parse-vpc-config vpc-config)})))
        arn))))

(defn parse-path [template argnames]
  (let [[_ path query] (re-matches #"(.*?)(\?.*)?" template)
        path-args (into #{} (map second) (re-seq #"\{([^}]*)}" path))
        query-arg-params (into {}
                           (when query (for [[_ param arg] (re-seq #"[&?](\w*)=\{(\w*)}" query)] [arg param])))
        arg-paths (into []
                    (comp (map name)
                      (map-indexed (fn [i ^String arg]
                                     (if (path-args arg)
                                       ["path" arg]
                                       (if-some [param (query-arg-params arg)]
                                         ["querystring" param]
                                         (cond
                                           (not (.startsWith arg "%")) (recur i (str "%" i))
                                           (= "%0" arg) (recur i "%")
                                           :else (throw (ex-info (str "Unmapped argument: " (nth argnames i)) {:template template :argnames argnames}))))))))
                    argnames)]
    {:path path
     :path-args path-args
     :query-args (vals query-arg-params)
     :arg-paths arg-paths}))

(defn mount-fn [f path {:keys [keeps content-type environment-variables vpc-config
                               arg-names lambda-function-name api-function-name
                               stage]
                        :or {content-type "application/json"
                             stage "repl"}}]
  (let [{:as parsed-path :keys [arg-paths]} (parse-path path arg-names)
        wrap (fn [in out ctx]
               (let [{:as method-request :strs [params]} (-> in slurp json/parse-string)
                     args (map #(get-in params %) arg-paths)]
                 (spit out (apply f args))))
        arn (deploy! wrap lambda-function-name
                     :keeps keeps
                     :environment-variables environment-variables
                     :vpc-config vpc-config)
        {:keys [region]} (aws/parse-arn arn)
        id (ensure-api lambda-function-name
                       api-function-name
                       (assoc parsed-path :content-type content-type))]
    (deploy-api! id stage)
    {:url (str "https://" id ".execute-api." region ".amazonaws.com/" stage (:path parsed-path))}))

(defmacro mount! [f path & {:as opts}]
  (if-some [var-f (cond 
                    (and (symbol? f) (not (contains? &env f)))
                    (list 'var f)
                    (and (seq? f) (= 'var (first f))) f)]
    `(mount-fn @~var-f
       ~(into {:arg-names `(-> ~var-f meta :arglists first)
               :lambda-function-name `(as-> (meta ~var-f) x# (str (:ns x#) "/" (:name x#)) (#'aws-name-munge x#))
               :api-function-name `(-> ~var-f meta :name name)
               :stage "repl"}
          opts))
    `(mount-fn ~f ~path ~opts)))

(defn invoke [var-f]
  (.invoke (build com.amazonaws.services.lambda.AWSLambdaClientBuilder)
           (donew com.amazonaws.services.lambda.model.InvokeRequest
                  {:function-name (as-> (meta var-f) x (str (:ns x) "/" (:name x)) (aws-name-munge x))})))



#_(deployment-package prn)

#_(def stub
   (reify com.amazonaws.services.lambda.runtime.RequestStreamHandler
     (handleRequest [_ in out ctx]
       (prn in out ctx))))

