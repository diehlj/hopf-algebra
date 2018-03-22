; TODO
; * include gorilla stuff
; * OPTIONS
;   - just latex stuff (then later get problems with graphs)
;   - put gorilla stuff now

(defproject hopf-algebra "0.2.0-SNAPSHOT"
  :description "A library to work with linear combinations of objects (usually they form a Hop algebra)."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [spyscope "0.1.5"] ]
  :injections [(require 'spyscope.core)]
  :plugins [[lein-gorilla "0.4.0"] ]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :repl {:dependencies [[gorilla-renderable "1.0.0"]
                                   [org.clojure/tools.namespace "0.2.11"]
                                   ]}
             :test {:dependencies [[gorilla-renderable "1.0.0"]
                                   ]}}
  )
