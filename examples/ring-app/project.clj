(defproject ring-app "0.1.0-SNAPSHOT"
  :description "Sample app using ring and portkey"
  :dependencies [[org.clojure/clojure "1.9.0-beta1"]
                 [compojure "1.6.0"]
                 [http-kit "2.2.0"]
                 [hiccup "1.0.5"]
                 [ring/ring-codec "1.0.1"]]
  :main ^:skip-aot ring-app.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :repl {:dependencies [[portkey "0.1.0-SNAPSHOT"]]
                    :source-paths ["dev"]
                    :repl-options {:init-ns dev}}})
