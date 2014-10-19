(defproject datil/schema-rosetta "0.1.0-SNAPSHOT"
  :description "Human-friendly "
  :url "http://github.com/datilmedia/schema-rosetta"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [prismatic/schema "0.3.0"]
                 [com.taoensso/tower "3.0.2"]]
  :deploy-repositories [["releases" :clojars]]
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.5.2"]]}})
