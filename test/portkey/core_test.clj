(ns portkey.core-test
  (:require [clojure.test :refer :all]
    [clojure.java.io :as io]
    [portkey.core :as pk]))

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
      (loop []
        (when-some [e (.getNextEntry zip)]
          (let [f (java.io.File. dir (.getName e))]
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

(deftest echo
  (testing "echo"
    (let [msg "hello world"
          zip (java.io.File/createTempFile "portkey-package" "zip")]
      (pk/package! zip (fn [in out ctx] (io/copy in out)))
      (let [^ClassLoader cl (create-class-loader (unzip zip))
            bos (java.io.ByteArrayOutputStream.)
            lambda (with-context-cl cl (.newInstance (.loadClass cl "portkey.LambdaStub")))]
        (is (not (instance? portkey.LambdaStub lambda))) ; checking isolation
        (with-context-cl cl
          (.handleRequest lambda (java.io.ByteArrayInputStream. (.getBytes msg "utf-8")) bos nil))
        (is (= msg (String. (.toByteArray bos) "utf-8")))))))
