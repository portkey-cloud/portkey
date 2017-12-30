(ns dev
  (:require [ring-app.handler :as handler]
            [portkey.core :as pk]
            [org.httpkit.server :as http]))

(defn start-local-server []
  (http/run-server #'handler/app {:port 3000}))

(defn deploy-lambda []
  (pk/mount-ring! #'handler/app :path "/greetings"))
