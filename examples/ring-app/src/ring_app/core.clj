(ns ring-app.core
  (:require [org.httpkit.server :as http]
            [ring-app.handler :refer [app]])
  (:gen-class))

(defn -main [& args]
  (http/run-server app))
