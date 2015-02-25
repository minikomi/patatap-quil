(defproject patatap "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [quil "2.2.5"]
                 [overtone/osc-clj "0.9.0"]
                 [processing-video "1.5.1-SNAPSHOT"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]

                 ]
  :profiles {:uberjar {:aot [patatap.core]}}
  
  :main patatap.core
  )
