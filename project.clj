(defproject clime "0.1.0-SNAPSHOT"
  :description "a micro template engine written in pure clojure"
  :url "https://github.com/zjhmale/clime"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :main ^:skip-aot clime.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
