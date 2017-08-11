(ns portkey.core-test
  (:require [clojure.test :refer :all]
    [clojure.java.io :as io]
    [portkey.core :as pk]
    [cemerick.pomegranate.aether :as mvn]
    [clojure.java.jdbc :as jdbc])
  (:import (com.opentable.db.postgres.embedded EmbeddedPostgres)))

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
  (let [args (into [] (filter symbol?) (keys &env))
        [requires body] (split-with #(and (seq? %) (= (first %) 'require)) body)]
    `(with-compiler-cl (create-deps-classloader '~deps (clojure.lang.RT/baseLoader))
       (binding [*ns* (find-ns '~(ns-name *ns*))]
         ((eval '(do ~@requires (fn ~args ~@body))) ~@args)))))

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

(deftest cheshire
  (testing "cheshire parse and generate"
    (is (= "{\"a\":1}"
           (with-deps [[cheshire "5.7.1"]]
             (do 
               (require 'cheshire.core)
               (invoke (fn [in out ctx]
                        (spit out (cheshire.core/generate-string (cheshire.core/parse-string (slurp in)))))
                      "{\"a\": 1}")))))))

(deftest timbre-slf4j
  (testing "timbre works"
    (is (not (empty?
              (with-deps [[com.taoensso/timbre "4.10.0"]]
                (require 'taoensso.timbre)
                (invoke (fn [in out ctx]
                          (spit out (with-out-str (taoensso.timbre/info "hello"))))))))))
  (testing "genclass doesn't bother us"
    (is (not (empty?
              (with-deps [[com.taoensso/timbre "4.10.0"]
                          [org.slf4j/slf4j-api "1.7.14"]
                          [com.fzakaria/slf4j-timbre "0.3.7"]]
                (invoke (fn [in out ctx]
                          (let [logger (org.slf4j.LoggerFactory/getLogger "test")]
                            (spit out (with-out-str (.info logger "world"))))))))))))

(deftest parse-path
  (is (= (pk/parse-path "/sum?x={x}&y={y}" ['x 'y])
         {:path "/sum"
          :path-args #{}
          :query-args '("x" "y")
          :arg-paths [["querystring" "x"] ["querystring" "y"]]})))

(defmacro with-db [& body]
  `(with-open [pg# (-> (EmbeddedPostgres/builder)
                       (.setPort 57664)
                       (.start))]
     (doseq [sql# ["create table foo (id int)"
                   "insert into foo values (1)"]]
       (jdbc/execute! {:datasource (.getPostgresDatabase pg#)} [sql#]))
     (do ~@body)))

(deftest jdbc
  (with-db
    (is (= "1"
           (with-deps [[org.postgresql/postgresql "42.1.3.jre7"]
                       [org.clojure/java.jdbc "0.7.0"]]
             (require '[clojure.java.jdbc :as jdbc])
             (binding [*extras* #{org.postgresql.geometric.PGcircle
                                  org.postgresql.geometric.PGline
                                  org.postgresql.geometric.PGpath
                                  org.postgresql.geometric.PGpolygon
                                  org.postgresql.geometric.PGlseg
                                  org.postgresql.util.PGmoney
                                  org.postgresql.util.PGInterval}]
               (invoke (fn [in out ctx]
                         (Class/forName "org.postgresql.Driver")
                         (->> (jdbc/query "jdbc:postgresql://localhost:57664/postgres?user=postgres"
                                          ["select * from foo"]
                                          {:result-set-fn first
                                           :row-fn :id})
                              str
                              (spit out))))))))))

(deftest amazonica
  (is (= "0"
         (with-deps [[amazonica "0.3.108"]]
           (require '[amazonica.aws.cloudwatch :as cw])
           (invoke (fn [in out ctx]
                     (spit out (-> (cw/list-metrics {:endpoint "eu-west-1"} :namespace "lol")
                                   :metrics
                                   count
                                   str))))))))
