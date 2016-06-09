(defproject slack-go "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"],
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [cheshire "5.6.1"]
                 [hiccup "1.0.5"]
                 [clj-http "2.2.0"]
                 [compojure "1.5.0"]
                 [ring "1.1.8"]
                 [ring/ring-defaults "0.1.5"]
                 [ring/ring-json "0.3.1"]
                 [org.clojars.pallix/batik "1.7.0"]
                 [the/parsatron "0.0.7"]]
  :main ^:skip-aot slack-go.core
  :plugins [[lein-ring "0.9.7"]]
  :ring {:handler slack-go.core/app}
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                                  [ring/ring-mock "0.3.0"]]}})
