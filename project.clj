(defproject computor-v1 "0.1.0-SNAPSHOT"
  :description "Polynom solver"
  :url "http://www.github.com/ftriquet/computor-v1"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot computor-v1.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
