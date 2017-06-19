(ns portkey.core-test
  (:require [clojure.test :refer :all]
    [clojure.java.io :as io]
    [portkey.core :as pk]
    [cemerick.pomegranate.aether :as mvn]))

(defn temp-dir [prefix]
  (let [p (java.io.File. (System/getProperty "java.io.tmpdir"))
        prefix (str prefix \- (System/currentTimeMillis) \-)]
    (loop [i 0]
      (let [tmp (java.io.File. p (str prefix i))]
        (cond
          (.mkdir tmp) tmp
          (< i 1000) (recur (inc i))
          :else (throw (java.io.IOException. "Can't create temporary dir.")))))))

(defn unzip [input]
  (with-open [in (io/input-stream input)
              zip (java.util.zip.ZipInputStream. in)]
    (let [dir (temp-dir "portkey-test")]
      (.deleteOnExit dir)
      (loop []
        (when-some [e (.getNextEntry zip)]
          (let [f (java.io.File. dir (.getName e))]
            (.deleteOnExit f)
            (if (.isDirectory e)
              (or (.mkdir f)
                (throw (ex-info (str "can't create dir " f) {:f f :e e})))
              (io/copy zip f))
          (.closeEntry zip)
          (recur))))
      dir)))

(defn create-classloader [^java.io.File dir]
  (let [urls (map #(-> ^java.io.File % .toURI .toURL)
               (cons dir (.listFiles (java.io.File. dir "lib"))))]
    (java.net.URLClassLoader. (into-array urls) nil)))

(defmacro with-compiler-cl [cl & body]
  `(with-bindings {clojure.lang.Compiler/LOADER ~cl} ~@body))

(defmacro with-context-cl [cl & body]
  `(let [cl# (.getContextClassLoader (Thread/currentThread))]
     (.setContextClassLoader (Thread/currentThread) ~cl)
     (try ~@body
       (finally
         (.setContextClassLoader (Thread/currentThread) cl#)))))

(defn create-deps-classloader [deps parent]
  (java.net.URLClassLoader.
    (into-array java.net.URL
      (map #(.toURL (.toURI ^java.io.File %))
        (mvn/dependency-files (mvn/resolve-dependencies :retrieve true :coordinates deps
                                :repositories (assoc mvn/maven-central "clojars" "http://clojars.org/repo")))))
    parent))

(defmacro with-deps [deps & body]
  (let [args (into [] (filter symbol?) (keys &env))]
    `(with-compiler-cl (create-deps-classloader '~deps (clojure.lang.RT/baseLoader))
       (binding [*ns* (find-ns '~(ns-name *ns*))]
         ((eval '(fn ~args ~@body)) ~@args)))))

(def ^:dynamic *extras* #{})

(defn invoke
  "Invokes f packaged as a lambda and isolated in a distinct class loader.
   Input and output are strings."
  ([f]
    (invoke f ""))
  ([f in-as-string]
    (let [zip (java.io.File/createTempFile "portkey-core-test" "zip")]
      (.deleteOnExit zip)
      (apply pk/package! zip f *extras*)
      (let [^ClassLoader cl (create-classloader (unzip zip))
            bos (java.io.ByteArrayOutputStream.)
            lambda (with-context-cl cl (.newInstance (.loadClass cl "portkey.LambdaStub")))]
        (when (instance? portkey.LambdaStub lambda) ; checking isolation
          (throw (ex-info "Containment has been breached." {})))
        (with-context-cl cl
          (.handleRequest lambda (java.io.ByteArrayInputStream. (.getBytes in-as-string "utf-8")) bos nil))
        (String. (.toByteArray bos) "utf-8")))))

(deftest echo
  (testing "echo"
    (let [msg "hello world"]
      (is (= msg (invoke (fn [in out ctx] (io/copy in out)) msg))))))

(deftest with-joda-time
  (testing "joda-time"
    (let [millis (System/currentTimeMillis)]
      (is (= millis
            (Long/parseLong
              (invoke (with-deps [[joda-time/joda-time "2.9.9"]]
                        (fn [in out ctx]
                          (spit out (.getMillis (org.joda.time.Instant. millis))))))))))))

(deftest with-hadoop-core
  (testing "hadoop-core"
    (let [dir "/tmp"
          file "1.txt"
          path "/tmp/1.txt"]
      (is (= path
            (with-deps [[org.apache.hadoop/hadoop-core "1.2.1"]]
              (invoke (fn [in out ctx]
                        (spit out (org.apache.hadoop.fs.Path. dir file))))))))))

(deftest parquet-avro
  (testing "parquet-avro"
    (is (= ""
           (with-deps [[org.apache.parquet/parquet-avro "1.9.0"]
                       [org.apache.hadoop/hadoop-core "1.2.1"]]
             (binding [*extras* #{org.apache.hadoop.security.ShellBasedUnixGroupsMapping
                                  org.apache.hadoop.security.UserGroupInformation$HadoopLoginModule
                                  org.apache.hadoop.fs.LocalFileSystem}]
               (invoke (fn [in out ctx]
                         (with-open [rdr (-> (org.apache.hadoop.fs.Path. "/dev/null")
                                           (org.apache.parquet.avro.AvroParquetReader/builder)
                                           (.withConf (doto (org.apache.hadoop.conf.Configuration.)
                                                        (.set "fs.file.impl" (.getName org.apache.hadoop.fs.LocalFileSystem))))
                                           (.build))]
                           (spit out (.read rdr)))))))))))
