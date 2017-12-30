(ns ring-app.handler
  (:require [compojure.core :refer :all]
            [hiccup.core :as h]
            [hiccup.form :as form]
            [ring.util.response :refer [response content-type]]
            [ring.middleware.params :refer [wrap-params]]))

(defroutes app
  (GET "/" []
    (-> (h/html
          [:html
           [:body
            [:h1 "Hello World"]
            (form/form-to [:post "/greet"]
                          (form/submit-button "Greet")
                          (form/text-field "name" ""))]])
        response
        (content-type "text/html")))
  (-> (POST "/greet" request
        (-> (str "Hi " (-> request :params (get "name")))
            response
            (content-type "text/html")))
      wrap-params))
