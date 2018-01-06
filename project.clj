(defproject hopf-algebra "0.1.0-SNAPSHOT"
  :description "A library to work with linear combinations of objects (usually they form a Hop algebra)."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.rpl/specter "0.13.1"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [clojure-future-spec "1.9.0-alpha14"]
                 [tupelo "0.1.71"]
                 ]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
