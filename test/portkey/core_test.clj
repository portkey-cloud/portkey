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

(defn create-class-loader [^java.io.File dir]
  (let [urls (map #(-> ^java.io.File % .toURI .toURL)
               (cons dir (.listFiles (java.io.File. dir "lib"))))]
    (java.net.URLClassLoader. (into-array urls) nil)))

(defmacro with-context-cl [cl & body]
  `(let [cl# (.getContextClassLoader (Thread/currentThread))]
     (.setContextClassLoader (Thread/currentThread) ~cl)
     (try ~@body
       (finally
         (.setContextClassLoader (Thread/currentThread) cl#)))))

(defn create-deps-class-loader [deps parent]
  (java.net.URLClassLoader.
    (into-array java.net.URL
      (map #(.toURL (.toURI ^java.io.File %))
        (mvn/dependency-files (mvn/resolve-dependencies :retrieve true :coordinates deps
                                :repositories (assoc mvn/maven-central "clojars" "http://clojars.org/repo")))))
    parent))

(defmacro with-deps [deps & body]
  (let [args (into [] (filter symbol?) (keys &env))]
    `(with-context-cl (create-deps-class-loader '~deps (.getContextClassLoader (Thread/currentThread)))
       ((eval '(fn ~args ~@body)) ~@args))))

(defn invoke
  "Invokes f packaged as a lambda and isolated in a distinct class loader.
   Input and output are strings."
  ([f]
    (invoke f ""))
  ([f in-as-string]
    (let [zip (java.io.File/createTempFile "portkey-core-test" "zip")]
      (.deleteOnExit zip)
      (pk/package! zip f)
      (let [^ClassLoader cl (create-class-loader (unzip zip))
            bos (java.io.ByteArrayOutputStream.)
            lambda (with-context-cl cl (.newInstance (.loadClass cl "portkey.LambdaStub")))]
        (assert (not (instance? portkey.LambdaStub lambda))) ; checking isolation
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
                        (spit out (.getMillis (org.joda.time.Instant. millis))))) "")))))))
